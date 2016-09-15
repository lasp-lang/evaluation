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
                    case Id == "client_server_delta_based_with_aae" orelse
                         Id == "peer_to_peer_delta_based_with_aae" orelse 
                         Id == "code_peer_to_peer_delta_based_with_aae" of
                        true ->
                            io:format("Analysing ~p~n", [EvalId]),
                            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
                            EvalTimestamps = only_dirs(EvalIdDir),

                            T = lists:foldl(
                                fun(EvalTimestamp, ToAverage0) ->
                                    OvercountingFile = EvalIdDir ++ "/" ++ EvalTimestamp ++ "/overcounting",
                                    Overcounting = get_overcounting(OvercountingFile),

                                    orddict:store(
                                        EvalTimestamp,
                                        Overcounting,
                                        ToAverage0
                                    )
                                end,
                                orddict:new(),
                                EvalTimestamps
                            ),

                            OvercountingAverage = average_overcounting(T),

                            PerClients0 = case orddict:find(Id, Acc) of
                                {ok, CPC} ->
                                    CPC;
                                error ->
                                    orddict:new()
                            end,

                            PerClients1 = orddict:store(list_to_integer(ClientNumber), OvercountingAverage, PerClients0),
                            orddict:store(Id, PerClients1, Acc);
                        false ->
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
    {InputFiles, Titles} = write_to_files(PlotDir, Map),
    OutputFile = output_file(PlotDir, "overcounting"),

    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating overcounting plot ~p. Output: ~p~n~n", [OutputFile, Result]).

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "overcounting.gnuplot".

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
get_overcounting(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),
    Line = io:get_line(FileDescriptor, ''),
    list_to_float(Line).

%% @private
average_overcounting(ToAverage) ->
    OvercountingSum = orddict:fold(
        fun(_Timestamp, Overcounting, Acc) ->
                Overcounting + Acc
        end,
        0,
        ToAverage
    ),

    OvercountingSum + orddict:size(ToAverage).

%% @private
write_to_files(PlotDir, Map) ->
    orddict:fold(
        fun(Id, ClientsToOvercounting, {InputFiles0, Titles0}) ->
            Title = get_title(Id),
            InputFile = PlotDir ++ Id,
            file:write_file(InputFile, "", [write]),

            lists:foreach(
                fun({Clients, Overcounting}) ->
                    append_to_file(InputFile, Clients, Overcounting)
                end,
                ClientsToOvercounting
            ),

            {[InputFile | InputFiles0], [Title | Titles0]}
        end,
        {[], []},
        Map
    ).

%% @private
get_title("client_server_delta_based_with_aae") -> "Client Server";
get_title("peer_to_peer_delta_based_with_aae") -> "Peer-to-Peer";
get_title("code_peer_to_peer_delta_based_with_aae") -> "Peer-to-Peer with Heavy Clients".

%% @private
append_to_file(InputFile, Clients, Overcounting) ->
    Line = io_lib:format("~w,~w\n", [Clients, Overcounting]),
    file:write_file(InputFile, Line, [append]).

%% @private
run_gnuplot(InputFiles, Titles, OutputFile) ->
    Bin = case os:getenv("MESOS_TASK_ID", "false") of
        "false" ->
            "gnuplot";
        _ ->
            "/usr/bin/gnuplot"
    end,
    Command = Bin ++ " -e \""
                  ++ "outputname='" ++ OutputFile ++ "'; "
                  ++ "inputnames='" ++ join_filenames(InputFiles) ++ "'; "
                  ++ "titles='" ++  join_titles(Titles) ++ "'\" " ++ gnuplot_file(),
    io:format("~p~n~n", [Command]),
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
            % "overcounting.gnuplot" does not support titles with spaces
            % But it converts all the "_" in the titles to " "
            Acc ++ re:replace(Elem, " ", "_", [global, {return, list}])
                ++ " "
        end,
        "",
        Titles
    ),
    string:strip(Line).
