#!/usr/bin/env escript

-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

main(_) ->
    ValidDirectories = ordsets:from_list(["divergence"]),

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
            IdMaxIndex = length(Tokens) - 6,
            EventIntervalIndex = length(Tokens) - 5,
            MaxEventsIndex = length(Tokens) - 4,
            BlockingSyncIndex = length(Tokens) - 3,
            StateIntervalIndex = length(Tokens) - 2,
            ClientNumberIndex = length(Tokens) - 1,
            PartitionProbabilityIndex = length(Tokens),
            EventInterval = lists:nth(EventIntervalIndex, Tokens),
            MaxEvents = lists:nth(MaxEventsIndex, Tokens),
            BlockingSync = lists:nth(BlockingSyncIndex, Tokens),
            StateInterval = lists:nth(StateIntervalIndex, Tokens),
            ClientNumber = list_to_integer(lists:nth(ClientNumberIndex, Tokens)),
            _MaxEvents = lists:nth(MaxEventsIndex, Tokens),
            _BlockingSync = lists:nth(BlockingSyncIndex, Tokens),
            _StateInterval = lists:nth(StateIntervalIndex, Tokens),
            _PartitionProbability = lists:nth(PartitionProbabilityIndex, Tokens),
            _HeavyClients = lists:nth(1, Tokens) == "code",
            Id = string:join(lists:sublist(Tokens, IdMaxIndex), "_"),

            io:format("Analysing ~p~n", [EvalId]),
            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
            EvalTimestamps = only_dirs(EvalIdDir),

            T = lists:foldl(
                fun(EvalTimestamp, ToAverage0) ->
                    EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,
                    Tuple = get_throughput_latency_and_divergence(EvalDir, EventInterval),

                    orddict:store(
                        EvalTimestamp,
                        Tuple,
                        ToAverage0
                    )
                end,
                orddict:new(),
                EvalTimestamps
            ),

            Tuple = average(T),

            MasterKey = {list_to_integer(MaxEvents),
                         list_to_atom(BlockingSync),
                         list_to_integer(StateInterval)},

            PerIds0 = case orddict:find(MasterKey, Acc) of
                {ok, PerIds} ->
                    PerIds;
                error ->
                    orddict:new()
            end,

            PerClients0 = case orddict:find(Id, PerIds0) of
                {ok, CPC} ->
                    CPC;
                error ->
                    orddict:new()
            end,

            PerClients1 = orddict:store(ClientNumber, Tuple, PerClients0),
            PerIds1 = orddict:store(Id, PerClients1, PerIds0),
            orddict:store(MasterKey, PerIds1, Acc)
        end,
        orddict:new(),
        EvalIds
    ),

    PlotDir = root_plot_dir() ++ "/" ++ Simulation ++ "/",
    filelib:ensure_dir(PlotDir),
    OutputFile = output_file(PlotDir, "divergence"),

    {InputFiles, Titles} = lists:foldl(
        fun({{MaxEvents, BlockingSync, StateInterval}=K, PerId}, {InputFiles0, Titles0}) ->
            lists:foldl(
                fun({Id, PerClient}, {InputFiles1, Titles1}) ->
                    Title = get_title(Id, K),
                    InputFile = PlotDir ++ Id ++ "_"
                             ++ integer_to_list(MaxEvents) ++ "_"
                             ++ atom_to_list(BlockingSync) ++ "_"
                             ++ integer_to_list(StateInterval),
            
		                %% truncate file
                    write_to_file(InputFile, ""),

                    lists:foreach(
                        fun({ClientNumber, {T, _L, D}}) ->
                            Line = float_to_list(T) ++ ","
                                ++ float_to_list(D) ++ ","
                                ++ integer_to_list(ClientNumber) ++ "\n",
                            append_to_file(InputFile, Line)
                        end,
                        PerClient
                    ),

                    {[InputFile | InputFiles0], [Title | Titles0]}
                end,
                {InputFiles0, Titles0},
                PerId
            )
        end,
        {[], []},
        Map
    ),

    generate_csv_for_r_analysis(PlotDir, Map),

    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating transmission plot ~p. Output: ~p~n~n", [OutputFile, Result]).

%% @private
get_title(_Id, K) -> get_title(K).
get_title("client_server_state_based_gcounter") -> "gcounter";
get_title("client_server_state_based_gset") -> "gset";
get_title("client_server_state_based_boolean") -> "boolean";
get_title("client_server_state_based_twopset") -> "add-only twopset";
get_title("client_server_state_based_awset_ps") -> "add-only provenance";
get_title({_, true, _}) -> "blocking";
get_title({_, false, Interval}) -> integer_to_list(Interval).

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "divergence.gnuplot".

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
            case re:run(Elem, ".*.csv$") of
                {match, _} ->
                    true;
                nomatch ->
                    false
            end
        end,
        LogFiles
    ).

%% @private
get_throughput_latency_and_divergence(EvalDir, EventInterval) ->
    LogFiles = only_csv_files(EvalDir),
    {Count, T, L} = lists:foldl(
        fun(File, {CAcc, TAcc, LAcc}=Acc) ->
            FilePath = EvalDir ++ "/" ++ File,
            {Ignore, T0, L0} =
                get_single_throughput_and_latency(FilePath, EventInterval),

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

    D = get_divergence(EvalDir),

    %% average divergence for this run
    {T, L / Count, D}.

%% @private
get_single_throughput_and_latency(FilePath, EventInterval) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %io:format("Processing file: ~p~n", [FilePath]),

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

            BatchSize = list_to_integer(Ops),
            TotalBatchTime = list_to_integer(Ms),
            Latency = TotalBatchTime - ((BatchSize - 1) * list_to_integer(EventInterval) / BatchSize),

            {
             Start1,
             list_to_integer(BatchEndMs),
             TotalOps0 + list_to_integer(Ops),
             BatchNumber0 + 1,
             BatchLatency0 + Latency
            }
        end,
        {undefined, undefined, 0, 0, 0},
        BatchLines
    ),

    case Ignore of
        true ->
            {true, 0, 0};
        false ->
            %io:format("TotalOps: ~p End: ~p, Start: ~p~n", [TotalOps, End, Start]),
            Diff = End - Start,
            T = TotalOps / (Diff / 1000),
            L = (BatchLatency / 1000) / BatchNumber,
            {Ignore, T, L}
    end.

%% @private
get_divergence(EvalDir) ->
    FilePath = EvalDir ++ "/overcounting",
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),
    Line = io:get_line(FileDescriptor, ''),
    list_to_float(Line).

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
average(ToAverage) ->
    {NumberOfExecutions, TSum, TList, LSum, DSum} = orddict:fold(
        fun(_Timestamp, {TSec, LSec, DSec}, {CountAcc, TAcc, TAccList, LAcc, DAcc}) ->
            {CountAcc + 1, TAcc + TSec, TAccList ++ [TSec], LAcc + LSec, DAcc + DSec}
        end,
        {0, 0, [], 0, 0},
        ToAverage
    ),

    io:format("[~p] ~p~n", [length(TList), TList]),

    {TSum / NumberOfExecutions, LSum / NumberOfExecutions, DSum / NumberOfExecutions}.

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

%% @private
generate_csv_for_r_analysis(PlotDir, Map) ->
    File = PlotDir ++ "r.csv",

    Header = "N,I,T,L,E\n",

    %% truncate file
    write_to_file(File, Header),

    lists:foreach(
        fun({{_, _, Interval}, [{_Sim, PerClients}]}) ->
            lists:foreach(
                fun({ClientNumber, {T, L, D}}) ->
                    List = [integer_to_list(ClientNumber),
                            integer_to_list(Interval),
                            float_to_list(T),
                            float_to_list(L),
                            float_to_list(D)],
                    Line = lists:flatten(lists:join(",", List)) ++ "\n",
                    append_to_file(File, Line)
                end,
                PerClients
            )
        end,
        Map
    ).
