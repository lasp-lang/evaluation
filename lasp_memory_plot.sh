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
    {AllTitles, AllInputFiles} = lists:foldl(
        fun(EvalId, {Titles0, InputFiles0}) ->
            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
            EvalTimestamps = only_dirs(EvalIdDir),

            T = lists:foldl(
                fun(EvalTimestamp, {Times0, ToAverage0}) ->
                    EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,

                    TimeToBytesAverage = generate_plot(EvalDir, Simulation, EvalId, EvalTimestamp),
                    Times2 = ordsets:union(Times0, get_times(TimeToBytesAverage)),

                    ToAverage1 = orddict:store(
                        EvalTimestamp,
                        TimeToBytesAverage,
                        ToAverage0
                    ),
                    {Times2, ToAverage1}

                end,
                {ordsets:new(), orddict:new()},
                EvalTimestamps
            ),
            
            {Title, InputFile} = generate_executions_average_plot(T, Simulation, EvalId),
            {[Title | Titles0], [InputFile | InputFiles0]}

        end,
        {[], []},
        EvalIds
    ),

    {{Titles, InputFiles}, {TitlesPS, InputFilesPS}} = lists:foldl(
        fun(Index, {{Titles0, InputFiles0}, {TitlesPS0, InputFilesPS0}}) ->
            Title = lists:nth(Index, AllTitles),
            InputFile = lists:nth(Index, AllInputFiles),
            case re:run(InputFile, ".*based_ps.*") of
                {match, _} ->
                    {
                        {Titles0, InputFiles0},
                        {
                            lists:append(TitlesPS0, [Title]),
                            lists:append(InputFilesPS0, [InputFile])
                        }
                    };
                nomatch ->
                    {
                        {
                            lists:append(Titles0, [Title]),
                            lists:append(InputFiles0, [InputFile])
                        },
                        {TitlesPS0, InputFilesPS0}
                    }
            end
        end,
        {{[], []}, {[], []}},
        lists:seq(1, length(AllTitles))
    ),
    
    PlotDir = root_plot_dir() ++ "/" ++ Simulation ++ "/",

    OutputFile = output_file(PlotDir, "multi_mode_memory"),
    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating multi-mode plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    OutputFilePS = output_file(PlotDir, "multi_mode_ps_memory"),
    ResultPS = run_gnuplot(InputFilesPS, TitlesPS, OutputFilePS),
    io:format("Generating multi-mode-ps plot ~p. Output: ~p~n~n", [OutputFilePS, ResultPS]),

    %% Remove input files
    delete_files(InputFiles),
    delete_files(InputFilesPS).

%% @private
generate_plot(EvalDir, Simulation, EvalId, EvalTimestamp) ->
    %%io:format("Will analyse the following directory: ~p~n~n", [EvalDir]),

    LogFiles = only_csv_files(EvalDir),
    %%io:format("Will analyse the following logs: ~p~n~n", [LogFiles]),

    {Map0, Times} = lists:foldl(
        fun(File, {Map0, Times0}) ->
            FilePath = EvalDir ++ "/" ++ File,

            %% Load this file to the map
            {Map1, Times1} = load_to_map(FilePath, Map0),

            %% Update set of times
            Times2 = ordsets:union(Times0, Times1),

            {Map1, Times2}
        end,
        {orddict:new(), ordsets:new()},
        LogFiles
    ),

    TimeZero = lists:min(Times),

    %% Assume unknown logs with last known values
    Map1 = assume_unknown_logs(Times, TimeZero, Map0),
    %%io:format("Unknown logs assumed!~n~n"),

    %% Write average in files (one file per type) to `PlotDir`
    PlotDir = root_plot_dir() ++ "/"
           ++ Simulation ++ "/"
           ++ EvalId ++ "/"
           ++ EvalTimestamp ++ "/",
    filelib:ensure_dir(PlotDir),

    generate_per_node_plot(Map1, PlotDir),
    TimeToBytesAverage = generate_nodes_average_plot(Times, Map1, PlotDir),
    TimeToBytesAverage.

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "memory.gnuplot".

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
load_to_map(FilePath, Map) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    lists:foldl(
        fun(Line, {Map0, Times0}) ->
            %% Parse log line
            [Type0, Time0, Bytes0] = string:tokens(Line, ",\n"),
            TypeA = list_to_atom(Type0),
            {TimeI, _} = string:to_integer(Time0),
            {BytesF, _} = string:to_float(Bytes0),

            Map1 = case TypeA of
                memory ->
                    %% Get dictionary that maps times to bytes
                    TimeToBytes0 = case orddict:find(FilePath, Map0) of
                        {ok, Value} ->
                            Value;
                        error ->
                            orddict:new()
                    end,
                    
                    TimeToBytes1 = orddict:store(TimeI, BytesF, TimeToBytes0),
                    orddict:store(FilePath, TimeToBytes1, Map0);
                _ ->
                    %% Ignore other logs
                    Map0
            end,

            %% Update set of times
            Times1 = ordsets:add_element(TimeI, Times0),

            {Map1, Times1}
        end,
        {Map, ordsets:new()},
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

%% @private
%% If in the logs of one node, we don't find some reference to some
%% time, assume the last known value
assume_unknown_logs(Times, TimeZero, Map) ->
    orddict:fold(
        fun(Node, TimeToLogs0, MapAcc) ->
            TimeToLogs1 = assume_per_node(TimeToLogs0, Times, TimeZero),
            orddict:store(Node, TimeToLogs1, MapAcc)
        end,
        orddict:new(),
        Map
    ).

%% @private
assume_per_node(TimeToBytes0, Times, TimeZero) ->
    LastKnownBytes = 0,

    {TimeToLogs, _} = lists:foldl(
        fun(Time, {TimeToBytesAcc, LastKnownBytesAcc}) ->
            Bytes = case orddict:find(Time, TimeToBytes0) of
                %% If logs exist for this time,
                %% use it, otherwise use the
                %% last known value
                {ok, V} ->
                    V;
                error ->
                    LastKnownBytesAcc
            end,

            %% Store `Time` minus `TimeZero`
            TimeToBytesAcc1 = orddict:store(Time - TimeZero, Bytes, TimeToBytesAcc),
            {TimeToBytesAcc1, Bytes}
        end,
        {orddict:new(), LastKnownBytes},
        Times
    ),
    TimeToLogs.

%% @private
generate_per_node_plot(Map, PlotDir) ->
    {Titles, InputFiles} = write_per_node_to_files(Map, PlotDir),
    OutputFile = output_file(PlotDir, "per_node_memory"),
    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating per node plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    %% Remove input files
    delete_files(InputFiles).

%% @private
write_per_node_to_files(Map, PlotDir) ->
    {Titles, InputFiles} = orddict:fold(
        fun(FileLogPath, TimeToBytes, {Titles0, InputFiles0}) ->
            NodeName = node_name(FileLogPath),
            Title = atom_to_list(memory) ++ "_" ++ NodeName,
            InputFile = PlotDir ++ Title ++ ".csv",

            lists:foreach(
                fun({Time, Bytes}) ->
                    append_to_file(InputFile, Time, Bytes)
                end,
                TimeToBytes
            ),

            {[Title | Titles0], [InputFile | InputFiles0]}
        end,
        {[], []},
        Map
    ),

    {Titles, InputFiles}.

%% @private
node_name(FileLogPath) ->
    Tokens = string:tokens(FileLogPath, "\/\."),
    NodeName = lists:nth(length(Tokens) - 1, Tokens),
    re:replace(NodeName, "@", "_", [global, {return, list}]).

%% @private
generate_nodes_average_plot(Times, Map, PlotDir) ->
    %% Do the average of `Map1`
    TimeToBytes = nodes_average(Times, Map),
    %%io:format("Average computed!~n~n"),

    InputFile = write_average_to_file(TimeToBytes, PlotDir),
    Title = "Memory",
    OutputFile = output_file(PlotDir, "average_memory"),
    Result = run_gnuplot([InputFile], [Title], OutputFile),
    io:format("Generating average plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    %% Remove input files
    delete_files([InputFile]),

    TimeToBytes.

%% @private
%% Do the average of all logs.
%% - Receives:
%%   * set of known times
%%   * a dictionary that maps nodes to dictionaries
%%     (from times to bytes)
%% - Produces a dictionary that maps times to bytes
nodes_average(Times, Map) ->
    TimeZero = lists:min(Times),
    Empty = create_empty_dict_time_to_bytes(
        lists:map(fun(Time) -> Time - TimeZero end, Times)
    ),

    TimeToBytesSum = orddict:fold(
        fun(_Node, Dict, Map1) ->
            orddict:fold(
                fun(Time, Bytes, Map2) ->
                    Current = orddict:fetch(Time, Map2),
                    orddict:store(Time, Current + Bytes, Map2)
                end,
                Map1,
                Dict
            )
        end,
        Empty,
        Map
    ),

    NodesNumber = orddict:size(Map),

    %% Divide each sum by the number of nodes
    orddict:map(
        fun(_Time, Sum) ->
            case Sum == 0 of
                true ->
                    Sum;
                false ->
                    Sum / NodesNumber
            end
        end,
        TimeToBytesSum
    ).

%% @private
create_empty_dict_time_to_bytes(Times) ->
    lists:foldl(
        fun(Time, Map0) ->
            orddict:store(Time, 0, Map0)
        end,
        orddict:new(),
        Times
    ).

%% @private
%% Write the average to file and return the name of the file.
write_average_to_file(TimeToBytes, PlotDir) ->
    InputFile = PlotDir ++ atom_to_list(memory) ++ ".csv",
    lists:foreach(
        fun({Time, Bytes}) ->
            append_to_file(InputFile, Time, Bytes)
        end,
        TimeToBytes
    ),
    InputFile.

%% @private
get_times(TimeToBytes) ->
    ordsets:from_list(
        orddict:fetch_keys(TimeToBytes)
    ).

%% @doc Average all executions
generate_executions_average_plot({Times, ToAverage}, Simulation, EvalId) ->
    Empty = create_empty_dict_time_to_bytes(Times),
    TimestampToLastKnown = lists:foldl(
        fun(Timestamp, Acc) ->
            orddict:store(Timestamp, 0, Acc)
        end,
        orddict:new(),
        orddict:fetch_keys(ToAverage)
    ),

    {TimeToBytesSum, _} = lists:foldl(
        %% For all the times
        fun(Time, Pair0) ->
            orddict:fold(
                %% For all the executions
                fun(Timestamp, TimeToBytes, {TimeToBytesSum0, TimestampToLastKnown0}) ->
                    Bytes = case orddict:find(Time, TimeToBytes) of
                        {ok, V} ->
                            V;
                        error ->
                            orddict:fetch(Timestamp, TimestampToLastKnown0)
                    end,

                    Current = orddict:fetch(Time, TimeToBytesSum0),
                    TimeToBytesSum1 = orddict:store(Time, Current + Bytes, TimeToBytesSum0),
                    TimestampToLastKnown1 = orddict:store(Timestamp, Bytes, TimestampToLastKnown0),
                    {TimeToBytesSum1, TimestampToLastKnown1}
                end,
                Pair0,
                ToAverage
            )
        end,
        {Empty, TimestampToLastKnown},
        Times
    ),

    %% Divide bytes by the number of executions
    NumberOfExecutions = length(orddict:fetch_keys(ToAverage)),
    TimeToBytesAverage = orddict:map(
        fun(_Time, Bytes) ->
            Bytes / NumberOfExecutions
        end,
        TimeToBytesSum
    ),

    PlotDir = root_plot_dir() ++ "/"
           ++ Simulation ++ "/"
           ++ EvalId ++ "/",
    filelib:ensure_dir(PlotDir),

    InputFile = write_average_to_file(TimeToBytesAverage, PlotDir),
    Title = "Memory",
    OutputFile = output_file(PlotDir, "average_memory"),
    Result = run_gnuplot([InputFile], [Title], OutputFile),
    io:format("Generating average plot of all executions ~p. Output: ~p~n~n", [OutputFile, Result]),
    
    {get_title(list_to_atom(EvalId)), InputFile}.

%% @private
get_title(aae_send)   -> "AAE Send";
get_title(delta_send) -> "Delta Send";
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

