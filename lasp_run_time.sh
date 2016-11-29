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
                                    RunTime = get_run_time(EvalDir),
                                    
                                    orddict:store(
                                        EvalTimestamp,
                                        RunTime,
                                        ToAverage0
                                    )
                                end,
                                orddict:new(),
                                EvalTimestamps
                            ),

                            AverageRunTimeSeconds = average_run_time(T),
                            io:format("T ~p\n\n", [T]),
                            io:format("Avg ~p\n\n", [AverageRunTimeSeconds]),
                            AverageRunTimeMinutes = AverageRunTimeSeconds / 60,

                            PerClients0 = case orddict:find(Id, Acc) of
                                {ok, CPC} ->
                                    CPC;
                                error ->
                                    orddict:new()
                            end,

                            PerClients1 = orddict:store(ClientNumber, AverageRunTimeMinutes, PerClients0),
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

    io:format("~p~n", [Map]).

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
average_run_time(T) ->
    Total = lists:foldl(
        fun({_Ts, Time}, Acc) ->
            Acc + Time
        end,
        0,
        T
    ),

    Total / orddict:size(T).

%% @private
get_run_time(EvalDir) ->
    LogFiles = only_csv_files(EvalDir),
    {StartTime, EndTime} = lists:foldl(
        fun(File, {Start0, End0}) ->
            FilePath = EvalDir ++ "/" ++ File,
            {Start1, End1} = get_node_run_time(FilePath),

            case Start1 /= -1 andalso End1 /= -1 of
                true ->
                    {Start1, End1};
                false ->
                    {Start0, End0}
            end
        end,
        {-1, -1},
        LogFiles
    ),

    case StartTime /= -1 andalso EndTime /= -1 of
        true ->
            ok;
        false ->
            io:format("There's something wrong!!!\n")
    end,

    EndTime - StartTime.

get_node_run_time(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),

    %% Ignore the first line
    [_ | Lines] = read_lines(FilePath, FileDescriptor),

    {StartTime, EndTime} = lists:foldl(
        fun(Line, {Start0, End0}) ->
            %% Parse log line
            [Type0, Time0, _Bytes0] = string:tokens(Line, ",\n"),
            TypeA = list_to_atom(Type0),
            TimeI = list_to_integer(Time0),

            case TypeA of
                experiment_started ->
                    {TimeI, End0};
                convergence ->
                    {Start0, TimeI};
                _ ->
                    {Start0, End0}
            end
        end,
        {-1, -1},
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
