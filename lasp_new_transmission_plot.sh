#!/usr/bin/env escript

-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

main(_) ->
    ValidDirectories = ordsets:from_list(["ad_counter"]),

    %% Filter out invalid directories
    Simulations0 = only_dirs(root_log_dir()),
    Simulations1 = lists:filter(
        fun(Simulation) ->
          ordsets:is_element(Simulation, ValidDirectories)
        end,
        Simulations0
    ),
            
    %% Generate plots
    lists:foreach(
        fun(Simulation) ->
          SimulationDir = root_log_dir() ++ "/" ++ Simulation,
          LocalAndDCOS = only_dirs(SimulationDir),
          lists:foreach(
            fun(Dir) ->
              Path = SimulationDir ++ "/" ++ Dir,
              EvalIds = only_dirs(Path),
              generate_plots(Simulation ++ "/" ++ Dir, EvalIds)
            end,
            LocalAndDCOS
          )
        end,
        Simulations1
    ).

%% @doc Generate plots.
generate_plots(Simulation, EvalIds) ->
    Map = lists:foldl(
        fun(EvalId, Acc) ->
            Tokens = string:tokens(EvalId, "_"),
            IdMaxIndex = length(Tokens) - 2,
            ClientNumberIndex = length(Tokens) - 1,
            PartitionProbabilityIndex = length(Tokens),
            ClientNumber = lists:nth(ClientNumberIndex, Tokens),
            PartitionProbability = lists:nth(PartitionProbabilityIndex, Tokens),
            Id = string:join(lists:sublist(Tokens, IdMaxIndex), "_"),

            case PartitionProbability of
                "0" ->
                    io:format("Analysing ~p~n", [EvalId]),
                    EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
                    EvalTimestamps = only_dirs(EvalIdDir),

                    T = lists:foldl(
                        fun(EvalTimestamp, ToAverage0) ->
                            EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,
                            {ServerMB, ClientsMB} = get_server_and_clients_mb(EvalDir),

                            orddict:store(
                                EvalTimestamp,
                                {ServerMB, ClientsMB},
                                ToAverage0
                            )
                        end,
                        orddict:new(),
                        EvalTimestamps
                    ),

                    {ServerMBAvg, ClientsMBAvg} = average_server_and_clients_mb(T),

                    orddict:store({ClientNumber, Id}, {ServerMBAvg, ClientsMBAvg}, Acc);
                _ ->
                    io:format("Ignoring ~p~n", [EvalId]),
                    Acc
            end
        end,
        orddict:new(),
        EvalIds
    ),

    io:format("~p~n", [Map]).

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "transmission.gnuplot".

%% @private
output_file(PlotDir, Name) ->
    PlotDir ++ Name ++ ".pdf".

%% @private
only_dirs(Dir) ->
    {ok, DirFiles} = file:list_dir(Dir),

    %% Ignore files
    lists:filter(
        fun(Elem) ->
            filelib:is_dir(Dir ++ "/" ++ Elem)
        end,
        DirFiles
    ).

%% @private
only_csv_files(LogDir) ->
    {ok, LogFiles} = file:list_dir(LogDir),

    %% Ignore not csv files
    lists:filter(
        fun(Elem) ->
            case re:run(Elem, ".*.csv") of
                {match, _} ->
                    true;
                nomatch ->
                    false
            end
        end,
        LogFiles
    ).

%% @private
get_server_and_clients_mb(EvalDir) ->
    LogFiles = only_csv_files(EvalDir),
    {ServerMB, ClientsMB} = lists:foldl(
        fun(File, {ServerAcc, ClientsAcc}) ->
            FilePath = EvalDir ++ "/" ++ File,

            {ServerOrClient, LastValue} = get_node_type_and_last_value(FilePath),

            case ServerOrClient of
                server ->
                    {ServerAcc + LastValue, ClientsAcc};
                client ->
                    {ServerAcc, ClientsAcc + LastValue}
            end
        end,
        {0, 0},
        LogFiles
    ),

    {ServerMB, ClientsMB}.

%% @private
get_node_type_and_last_value(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    lists:foldl(
        fun(Line, {ServerOrClient0, LastValue0}) ->
            %% Parse log line
            [Type0, _Time0, Bytes0] = string:tokens(Line, ",\n"),
            TypeA = list_to_atom(Type0),
            {BytesF, _} = string:to_float(Bytes0),

            case TypeA of
                memory ->
                    %% Ignore memory logs
                    {ServerOrClient0, LastValue0};
                experiment_started ->
                    {server, LastValue0};
                convergence ->
                    %% Ignore convergence logs
                    {ServerOrClient0, LastValue0};
                _ ->
                    %% TODO this assumes there's only one other type of logs
                    {ServerOrClient0, BytesF}
            end
        end,
        {client, 0},
        Lines
    ).

%% @private
read_lines(FilePath, FileDescriptor) ->
    case io:get_line(FileDescriptor, '') of
        eof ->
            [];
        {error, Error} ->
            lager:warning("Error while reading line from file ~p. Error: ~p", [FilePath, Error]),
            [];
        Line ->
            [Line | read_lines(FilePath, FileDescriptor)]
    end.

%% @private
append_to_file(InputFile, Time, Bytes) ->
    Line = io_lib:format("~w,~w\n", [Time, Bytes]),
    file:write_file(InputFile, Line, [append]).

%% @doc Average all executions
average_server_and_clients_mb(ToAverage) ->
    {NumberOfExecutions, ServerMBSum, ClientsMBSum} = orddict:fold(
        fun(_Timestamp, {ServerMB, ClientsMB}, {CountAcc, ServerAcc, ClientsAcc}) ->
            {CountAcc + 1, ServerAcc + ServerMB, ClientsAcc + ClientsMB}
        end,
        {0, 0, 0},
        ToAverage
    ),

    {ServerMBSum / NumberOfExecutions, ClientsMBSum / NumberOfExecutions}.

%% @private
get_titles(Types) ->
    lists:map(
        fun(Type) ->
            get_title(Type)
        end,
        Types
    ).

%% @private
get_title(aae_send)   -> "AAE Send";
get_title(delta_send) -> "Delta Send";
get_title(broadcast) -> "Broadcast";
get_title(peer_to_peer_state_based_with_aae)              -> "P2P - State Based";
get_title(peer_to_peer_state_based_with_aae_and_tree)     -> "P2P - State Based + tree";
get_title(peer_to_peer_delta_based_with_aae)              -> "P2P - Delta based";
get_title(peer_to_peer_state_based_ps_with_aae)           -> "P2P - State Based PS";
get_title(peer_to_peer_state_based_ps_with_aae_and_tree)  -> "P2P - State Based PS + tree";
get_title(peer_to_peer_delta_based_ps_with_aae)           -> "P2P - Delta based PS";
get_title(client_server_state_based_with_aae)             -> "CS - State Based";
get_title(client_server_state_based_with_aae_and_tree)    -> "CS - State Based + tree";
get_title(client_server_delta_based_with_aae)             -> "CS - Delta based";
get_title(client_server_state_based_ps_with_aae)          -> "CS - State Based PS";
get_title(client_server_state_based_ps_with_aae_and_tree) -> "CS - State Based PS + tree";
get_title(client_server_delta_based_ps_with_aae)          -> "CS - Delta based PS".

%% @private
run_gnuplot(InputFiles, Titles, OutputFile, ConvergenceTime) ->
    Bin = case os:getenv("MESOS_TASK_ID", "false") of
        "false" ->
            "gnuplot";
        _ ->
            "/usr/bin/gnuplot"
    end,
    Command = Bin ++ " -e \""
                  ++ "convergence_time='" ++ integer_to_list(ConvergenceTime) ++ "'; "
                  ++ "outputname='" ++ OutputFile ++ "'; "
                  ++ "inputnames='" ++ join_filenames(InputFiles) ++ "'; "
                  ++ "titles='" ++  join_titles(Titles) ++ "'\" " ++ gnuplot_file(),
    %%io:format("~p~n~n", [Command]),
    os:cmd(Command).

%% @private
join_filenames(InputFiles) ->
    Line = lists:foldl(
        fun(Elem, Acc) ->
            Acc ++ Elem
                ++ " "
        end,
        "",
        InputFiles
    ),
    string:strip(Line).

%% @private
join_titles(Titles) ->
    Line = lists:foldl(
        fun(Elem, Acc) ->
            % "transmission.gnuplot" does not support titles with spaces
            % But it converts all the "_" in the titles to " "
            Acc ++ re:replace(Elem, " ", "_", [global, {return, list}])
                ++ " "
        end,
        "",
        Titles
    ),
    string:strip(Line).

%% @private
delete_files(Files) ->
    lists:foreach(
      fun(File) ->
        ok = file:delete(File)
      end,
      Files
    ).

