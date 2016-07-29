#!/usr/bin/env escript

-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

main(_) ->
    ValidDirectories = ordsets:from_list(["path_contraction"]),

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
          EvalIds = only_dirs(SimulationDir),
          generate_plots(Simulation, EvalIds)
        end,
        Simulations1
    ).

%% @doc Generate plots.
generate_plots(Simulation, EvalIds) ->
    {Titles, InputFiles} = lists:foldl(
        fun(EvalId, {Titles0, InputFiles0}) ->
            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
            EvalTimestamps = only_dirs(EvalIdDir),

            ToAverage = lists:foldl(
                fun(EvalTimestamp, ToAverage0) ->
                    EvalDir = EvalIdDir ++ "/" ++ EvalTimestamp,
                    IterationToValue = generate_plot(EvalDir, Simulation, EvalId, EvalTimestamp),

                    orddict:store(
                        EvalTimestamp,
                        IterationToValue,
                        ToAverage0
                    )
                end,
                orddict:new(),
                EvalTimestamps
            ),

            Title = get_title(list_to_atom(EvalId)),
            InputFile = generate_executions_average_plot(ToAverage, Simulation, EvalId),

            {[Title | Titles0], [InputFile | InputFiles0]}
        end,
        {[], []},
        EvalIds
    ),

    PlotDir = root_plot_dir() ++ "/" ++ Simulation ++ "/",

    OutputFile = output_file(PlotDir, "multi-mode"),
    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating multi-mode plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    delete_files(InputFiles).

%% @private
generate_plot(EvalDir, Simulation, EvalId, EvalTimestamp) ->
    %%io:format("Will analyse the following directory: ~p~n~n", [EvalDir]),

    LogFiles = only_csv_files(EvalDir),
    %%io:format("Will analyse the following logs: ~p~n~n", [LogFiles]),

    Map1 = lists:foldl(
        fun(File, Map0) ->
            FilePath = EvalDir ++ "/" ++ File,

            %% Get list of times
            List = get_time_list(FilePath),

            orddict:store(FilePath, List, Map0)
        end,
        orddict:new(),
        LogFiles
    ),

    %% Write average in files (one file per type) to `PlotDir`
    PlotDir = root_plot_dir() ++ "/"
           ++ Simulation ++ "/"
           ++ EvalId ++ "/"
           ++ EvalTimestamp ++ "/",
    filelib:ensure_dir(PlotDir),

    generate_per_node_plot(Map1, PlotDir),

    IterationToValue = generate_nodes_average_plot(Map1, PlotDir),
    IterationToValue.

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "path_contraction.gnuplot".

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
get_time_list(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Get lines
    Lines = read_lines(FilePath, FileDescriptor),

    lists:foldl(
        fun(Line, List) ->
            %% Parse log line
            [Time0] = string:tokens(Line, "\n"),
            {TimeI, _} = string:to_integer(Time0),

            lists:append(List, [TimeI])
        end,
        [],
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
append_to_file(InputFile, Time) ->
    Line = io_lib:format("~w\n", [Time]),
    file:write_file(InputFile, Line, [append]).

%% @private
generate_per_node_plot(Map, PlotDir) ->
    {Titles, InputFiles} = write_per_node_to_files(Map, PlotDir),
    OutputFile = output_file(PlotDir, "per_node"),
    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating per node plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    %% Remove input files
    delete_files(InputFiles).

%% @private
write_per_node_to_files(Map, PlotDir) ->
    {Titles, InputFiles} = orddict:fold(
        fun(FileLogPath, TimeList, {Titles0, InputFiles0}) ->
            NodeName = node_name(FileLogPath),
            InputFile = PlotDir ++ NodeName ++ ".csv",

            lists:foreach(
                fun(Time) ->
                    append_to_file(InputFile, Time)
                end,
                TimeList
            ),

            {[NodeName | Titles0], [InputFile | InputFiles0]}
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
generate_nodes_average_plot(Map0, PlotDir) ->
    %% Do the average of `Map0`
    IterationToValue = nodes_average(Map0),
    %%io:format("Average computed!~n~n"),

    InputFile = PlotDir ++ "node_average.csv", 
    Title = "Node Average",
    OutputFile = output_file(PlotDir, "node_average"),

    write_average_to_file(IterationToValue, InputFile),

    Result = run_gnuplot([InputFile], [Title], OutputFile),
    io:format("Generating average plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    %% Remove input files
    delete_files([InputFile]),

    IterationToValue.

%% @private
%% Do the average of nodes logs.
nodes_average(Map) ->
    [{_, FirstList} | _] = Map,
    Iterations = length(FirstList),

    IterationToValue0 = create_empty_iteration_to_value(Iterations),

    IterationToValue1 = orddict:fold(
        fun(_FilePath, NodeList, Result0) ->
            lists:foldl(
                fun(Iteration, Result1) ->
                    CurrentValue = orddict:fetch(Iteration, Result1),
                    NodeValue = lists:nth(Iteration, NodeList),
                    orddict:store(Iteration, CurrentValue + NodeValue, Result1)
                end,
                Result0,
                lists:seq(1, Iterations)
            )
        end,
        IterationToValue0,
        Map
    ),

    IterationToValue1.

%% @private
create_empty_iteration_to_value(Iterations) ->
    lists:foldl(
        fun(Iteration, IterationToValue) ->
            orddict:store(Iteration, 0, IterationToValue)
        end,
        orddict:new(),
        lists:seq(1, Iterations)
    ).
    
%% @private
%% Write the average to file and return the name of the file.
write_average_to_file(IterationToValue, InputFile) ->
    lists:foreach(
        fun({_Iteration, Time}) ->
            append_to_file(InputFile, Time)
        end,
        IterationToValue
    ).

%% @doc Average all executions
%%      ToAverage :: EvalTimestamp -> (Iteration -> Value)
generate_executions_average_plot(ToAverage, Simulation, EvalId) ->
    [{_, FirstList} | _] = ToAverage,
    Iterations = length(FirstList),

    IterationToValue0 = create_empty_iteration_to_value(Iterations),

    IterationToValue1 = orddict:fold(
        fun(_EvalTimestamp, IterationToValue, Result0) ->
        		orddict:fold(
                fun(Iteration, Time, Result1) ->
                    CurrentValue = orddict:fetch(Iteration, Result1),
                    orddict:store(Iteration, CurrentValue + Time, Result1)
                end,
                Result0,
                IterationToValue
            )
        end,
        IterationToValue0,
        ToAverage
    ),

    %% Divide bytes by the number of executions
    NumberOfExecutions = length(orddict:fetch_keys(ToAverage)),
    IterationToValue2 = orddict:map(
        fun(_Itearation, Time) ->
            Time / NumberOfExecutions
        end,
        IterationToValue1
    ),

    PlotDir = root_plot_dir() ++ "/"
           ++ Simulation ++ "/"
           ++ EvalId ++ "/",
    filelib:ensure_dir(PlotDir),
    
    InputFile = PlotDir ++ "average.csv",
    Title = "Average",
    OutputFile = output_file(PlotDir, "average"),

    write_average_to_file(IterationToValue2, InputFile),

    Result = run_gnuplot([InputFile], [Title], OutputFile),
    io:format("Generating average plot of all executions ~p. Output: ~p~n~n", [OutputFile, Result]),

    InputFile.

%% @private
get_title(contraction) -> "Contraction";
get_title(no_contraction) -> "No Contraction";
get_title(contraction_with_reads) -> "Contraction with random reads".

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
            % "path_contraction.gnuplot" does not support titles with spaces
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

