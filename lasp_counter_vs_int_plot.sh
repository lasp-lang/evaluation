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
                                    Tuple = get_counter_and_int_mb(EvalDir),
                                    
                                    orddict:store(
                                        EvalTimestamp,
                                        Tuple,
                                        ToAverage0
                                    )
                                end,
                                orddict:new(),
                                EvalTimestamps
                            ),

                            Tuple = average_counter_and_int_mb(T),

                            PerClients0 = case orddict:find(Id, Acc) of
                                {ok, CPC} ->
                                    CPC;
                                error ->
                                    orddict:new()
                            end,

                            PerClients1 = orddict:store(ClientNumber, Tuple, PerClients0),
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

    Header = "ABCXYZ,32_s,64_s\n",
    L1 = io_lib:format("gcounter,~w,~w",
                       [
                        gb(element(1, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(1, orddict:fetch("64", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map))))
                       ]),
    L2 = io_lib:format("max_int,~w,~w\n",
                       [
                        gb(element(2, orddict:fetch("32", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map)))),
                        gb(element(2, orddict:fetch("64", orddict:fetch("peer_to_peer_state_based_with_aae_test", Map))))
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
    "counter_vs_int.gnuplot".

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
get_counter_and_int_mb(EvalDir) ->
    LogFiles = only_csv_files(EvalDir),
    lists:foldl(
        fun(File, {CounterAcc, MaxIntAcc}) ->
            FilePath = EvalDir ++ "/" ++ File,
            {Counter, MaxInt} = get_counter_and_int_transmission(FilePath),

            {CounterAcc + Counter, MaxIntAcc + MaxInt}
        end,
        {0, 0},
        LogFiles
    ).

%% @private
get_counter_and_int_transmission(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    LogTypeToLastBytes = lists:foldl(
        fun(Line, Acc) ->
            %% Parse log line
            case length(string:tokens(Line, ",\n")) == 3 of
                true ->
                    [Type0, _Time0, Bytes0] = string:tokens(Line, ",\n"),
                    {BytesF, _} = string:to_float(Bytes0),

                    orddict:store(Type0, BytesF, Acc);
                false ->
                    %% IGNORE lines that start with
                    %% {gmap,[{pair,[boolean,boolean]}]}
                    Acc
            end
        end,
        orddict:new(),
        Lines
    ),

    lists:foldl(
        fun({LogType, Bytes}, {CounterAcc, MaxIntAcc}) ->
            case LogType of
                "gcounter" ->
                    {CounterAcc + Bytes, MaxIntAcc};
                "state_max_int" ->
                    {CounterAcc, MaxIntAcc + Bytes};
                _ ->
                    {CounterAcc, MaxIntAcc}
            end
        end,
        {0, 0},
        LogTypeToLastBytes
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
write_to_file(InputFile, Line) ->
    file:write_file(InputFile, Line, [write]).

%% @private
append_to_file(InputFile, Line) ->
    file:write_file(InputFile, Line, [append]).

%% @doc Average all executions
average_counter_and_int_mb(ToAverage) ->
    {NumberOfExecutions, CounterSum, MaxIntSum} = orddict:fold(
        fun(_Timestamp, {CounterMB, MaxIntMB}, {CountAcc, CounterAcc, MaxIntAcc}) ->
            {CountAcc + 1, CounterAcc + CounterMB, MaxIntAcc + MaxIntMB}
        end,
        {0, 0, 0},
        ToAverage
    ),

    {CounterSum / NumberOfExecutions, MaxIntSum / NumberOfExecutions}.

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
