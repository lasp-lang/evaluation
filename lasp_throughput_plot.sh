#!/usr/bin/env escript

-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

main(_) ->
    ValidDirectories = ordsets:from_list(["throughput"]),

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

            io:format("Analysing ~p~n", [EvalId]),
            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
            EvalTimestamps = only_dirs(EvalIdDir),

            T = lists:foldl(
                fun(EvalTimestamp, ToAverage0) ->
                    EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,
                    Tuple = get_throughput_and_latency(EvalDir),

                    orddict:store(
                        EvalTimestamp,
                        Tuple,
                        ToAverage0
                    )
                end,
                orddict:new(),
                EvalTimestamps
            ),

            Tuple = average_throughput_and_latency(T),

            PerClients0 = case orddict:find(Id, Acc) of
                {ok, CPC} ->
                    CPC;
                error ->
                    orddict:new()
            end,

            PerClients1 = orddict:store(ClientNumber, Tuple, PerClients0),
            orddict:store(Id, PerClients1, Acc)
        end,
        orddict:new(),
        EvalIds
    ),

    io:format("~p~n", [Map]),

    PlotDir = root_plot_dir() ++ "/" ++ Simulation ++ "/",
    filelib:ensure_dir(PlotDir),
    InputFile = PlotDir ++ "throughput",
    OutputFile = output_file(PlotDir, "throughput"),

		%% truncate file
    write_to_file(InputFile, ""),
    lists:foreach(
        fun({ClientNumber, {T, L}}) ->
            Line = float_to_list(T) ++ ","
                ++ float_to_list(L) ++ ","
                ++ ClientNumber ++ "\n",
            append_to_file(InputFile, Line)
        end,
        orddict:fetch("peer_to_peer_throughput_state_based", Map)
    ),

    Result = run_gnuplot([InputFile], ["star block sync"], OutputFile),
    io:format("Generating transmission plot ~p. Output: ~p~n~n", [OutputFile, Result]).

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "throughput.gnuplot".

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
get_throughput_and_latency(EvalDir) ->
    LogFiles = only_csv_files(EvalDir),
    {Count, T, L} = lists:foldl(
        fun(File, {CAcc, TAcc, LAcc}=Acc) ->
            FilePath = EvalDir ++ "/" ++ File,
            {Ignore, T0, L0} = get_single_throughput_and_latency(FilePath),

            case Ignore of
                true ->
                    Acc;
                false ->
                    {CAcc + 1, TAcc + T0, LAcc + L0}
            end
        end,
        {0, 0, 0},
        LogFiles
    ),

    %% average throughput and latency for this run
    {T, L / Count}.

%% @private
get_single_throughput_and_latency(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    {Ignore, BatchLines} = lists:foldl(
        fun(Line, {Ignore0, BatchLines0}=Acc) ->
            [Type0, _Time0, BatchLine] = string:tokens(Line, ";\n"),
            case list_to_atom(Type0) of
                experiment_started ->
                    %% Ignore server logs
                    {true, BatchLines0};
                batch ->
                    {Ignore0, BatchLines0 ++ [BatchLine]};
                _ ->
                    Acc
            end
        end,
        {false, []},
        Lines
    ),

    {Start, End, TotalOps, BatchNumber, BatchLatency} = lists:foldl(
        fun(Line, {Start0, _End0, TotalOps0, BatchNumber0, BatchLatency0}) ->
            [BatchStartMs, BatchEndMs, Ops, Ms] = string:tokens(Line, ","),

            Start1 = case Start0 of
                undefined ->
                    %% if not defined, use the first batch
                    %% start time
                    list_to_integer(BatchStartMs);
                _ ->
                    Start0
            end,

            {
             Start1,
             list_to_integer(BatchEndMs),
             TotalOps0 + list_to_integer(Ops),
             BatchNumber0 + 1, 
             BatchLatency0 + (list_to_integer(Ms) / list_to_integer(Ops))
            }
        end,
        {undefined, undefined, 0, 0, 0},
        BatchLines
    ),

    case Ignore of
        true ->
            {true, 0, 0};
        false ->
            Diff = End - Start,
            T = TotalOps / (Diff / 1000),
            L = (BatchLatency / 1000) / BatchNumber,
            {Ignore, T, L}
    end.

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
average_throughput_and_latency(ToAverage) ->
    {NumberOfExecutions, TSum, LSum} = orddict:fold(
        fun(_Timestamp, {TSec, LSec}, {CountAcc, TAcc, LAcc}) ->
            {CountAcc + 1, TAcc + TSec, LAcc + LSec}
        end,
        {0, 0, 0},
        ToAverage
    ),

    {TSum / NumberOfExecutions, LSum / NumberOfExecutions}.

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
            % "*.gnuplot" does not support titles with spaces
            % But it converts all the "_" in the titles to " "
            Acc ++ re:replace(Elem, " ", "_", [global, {return, list}])
                ++ " "
        end,
        "",
        Titles
    ),
    string:strip(Line).
