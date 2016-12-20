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
            HeavyClients = lists:nth(1, Tokens) == "code",
            Id = string:join(lists:sublist(Tokens, IdMaxIndex), "_"),

            case PartitionProbability of
                "0" ->
                    case HeavyClients of
                        false ->
                            io:format("Analysing ~p~n", [EvalId]),
                            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
                            EvalTimestamps = only_dirs(EvalIdDir),

                            T = lists:foldl(
                                fun(EvalTimestamp, ToAverage0) ->
                                    EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,
                                    %% Before
                                    %% {ServerMB, ClientsMB} = get_server_and_clients_mb(EvalDir),
                                    %% Now
                                    {ServerMB, ClientsMB} = get_state_and_protocol_mb(EvalDir),
                                    
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

                            PerClients0 = case orddict:find(Id, Acc) of
                                {ok, CPC} ->
                                    CPC;
                                error ->
                                    orddict:new()
                            end,

                            PerClients1 = orddict:store(ClientNumber, {ServerMBAvg, ClientsMBAvg}, PerClients0),
                            orddict:store(Id, PerClients1, Acc);
                        true ->
                            io:format("Ignoring ~p~n", [EvalId]),
                            Acc
                    end;
                _ ->
                    io:format("Ignoring ~p~n", [EvalId]),
                    Acc
            end
        end,
        orddict:new(),
        EvalIds
    ),

    io:format("~p~n", [Map]),

    PlotDir = root_plot_dir() ++ "/" ++ Simulation ++ "/",
    filelib:ensure_dir(PlotDir),
    InputFile = PlotDir ++ "transmission",
    OutputFile = output_file(PlotDir, "transmission"),

    Header = "ABCXYZ,8_s,8_c,16_s,16_c,32_s,32_c\n",
    L1 = io_lib:format("hg-lasp,~w,~w,~w,~w\n",
                       [
                        gb(element(1, orddict:fetch("8", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(2, orddict:fetch("8", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(1, orddict:fetch("16", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(2, orddict:fetch("16", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(1, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(2, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map))))
                       ]),
    L2 = io_lib:format("hg-lasp-tree,~w,~w,~w,~w\n",
                       [
                        gb(element(1, orddict:fetch("8", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map)))),
                        gb(element(2, orddict:fetch("8", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map)))),
                        gb(element(1, orddict:fetch("16", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map)))),
                        gb(element(2, orddict:fetch("16", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map)))),
                        gb(element(1, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map)))),
                        gb(element(2, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_and_tree_test", Map))))
                       ]),
    %% truncate file
    write_to_file(InputFile, ""),
    append_to_file(InputFile, Header),
    append_to_file(InputFile, L1),
    append_to_file(InputFile, L2),

    Result = run_gnuplot(InputFile, OutputFile),
    io:format("Generating transmission plot ~p. Output: ~p~n~n", [OutputFile, Result]).

%% @private
gb(MB) ->
    MB / 1024.

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "new_transmission.gnuplot".

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
get_state_and_protocol_mb(EvalDir) ->
    LogFiles = only_csv_files(EvalDir),
    {StateMB, ProtocolMB} = lists:foldl(
        fun(File, {StateAcc, ProtocolAcc}) ->
            FilePath = EvalDir ++ "/" ++ File,
            {State, Protocol} = get_state_and_protocol_transmission(FilePath),

            {StateAcc + State, ProtocolAcc + Protocol}
        end,
        {0, 0},
        LogFiles
    ),

    {StateMB, ProtocolMB}.

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
get_state_and_protocol_transmission(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    LogTypeToLastBytes = lists:foldl(
        fun(Line, Acc) ->
            %% Parse log line
            [Type0, _Time0, Bytes0] = string:tokens(Line, ",\n"),
            TypeA = list_to_atom(Type0),
            {BytesF, _} = string:to_float(Bytes0),

            orddict:store(TypeA, BytesF, Acc)
        end,
        orddict:new(),
        Lines
    ),

    {State, Protocol} = lists:foldl(
        fun({LogType, Bytes}, {StateAcc, ProtocolAcc}) ->
            case log_type(LogType) of
                state ->
                    {StateAcc + Bytes, ProtocolAcc};
                protocol ->
                    {StateAcc, ProtocolAcc + Bytes};
                ignore ->
                    {StateAcc, ProtocolAcc}
            end
        end,
        {0, 0},
        LogTypeToLastBytes
    ),

    {State, Protocol}.

%% @private
log_type(memory) -> ignore;
log_type(convergence) -> ignore;
log_type(experiment_started) -> ignore;
log_type(aae_send) -> state;
log_type(aae_send_protocol) -> protocol;
log_type(delta_send) -> state;
log_type(delta_send_protocol) -> protocol;
log_type(broadcast) -> state;
log_type(broadcast_protocol) -> protocol.
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
write_to_file(InputFile, Line) ->
    file:write_file(InputFile, Line, [write]).

%% @private
append_to_file(InputFile, Line) ->
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
run_gnuplot(InputFile, OutputFile) ->
    Bin = case os:getenv("MESOS_TASK_ID", "false") of
        "false" ->
            "gnuplot";
        _ ->
            "/usr/bin/gnuplot"
    end,
    Command = Bin ++ " -e \""
                  ++ "outputname='" ++ OutputFile ++ "'; "
                  ++ "inputname='" ++  InputFile ++ "'\" " ++ gnuplot_file(),
    io:format("~p~n~n", [Command]),
    os:cmd(Command).
