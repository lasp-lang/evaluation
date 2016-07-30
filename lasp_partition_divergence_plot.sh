#!/usr/bin/env escript

-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

main(_) ->
    ValidDirectories = ordsets:from_list(["ad_counter_partition_divergence"]),

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
    {ClientServer, PeerToPeer, CodePeerToPeer} = lists:foldl(
        fun(EvalId, {CS0, PP0, CPP0}) ->
            CPP1 = case lists:prefix("code_peer_to_peer_", EvalId) of
                true ->
                    [EvalId | CPP0];
                false ->
                    CPP0
            end,
            PP1 = case lists:prefix("peer_to_peer", EvalId) of
                true ->
                    [EvalId | PP0];
                false ->
                    PP0
            end,
            CS1 = case lists:prefix("client_server", EvalId) of
                true ->
                    [EvalId | CS0];
                false ->
                    CS0
            end,

            {CS1, PP1, CPP1}
        end,
        {[], [], []},
        EvalIds
    ),

    ClientServerAvg = average(Simulation, ClientServer),
    PeerToPeerAvg = average(Simulation, PeerToPeer),
    CodePeerToPeerAvg = average(Simulation, CodePeerToPeer),

    io:format("Client Server Average ~p~n", [ClientServerAvg]),
    io:format("Peer-to-Peer Average ~p~n", [PeerToPeerAvg]),
    io:format("Code Peer-to-Peer Average ~p~n", [CodePeerToPeerAvg]),

    InputFile1 = write_to_file(Simulation, "client_server", ClientServerAvg),
    InputFile2 = write_to_file(Simulation, "peer_to_peer", PeerToPeerAvg),
    InputFile3 = write_to_file(Simulation, "code_peer_to_peer", CodePeerToPeerAvg),

    InputFiles = [InputFile1, InputFile2, InputFile3],
    Titles = ["Client Server", "Peer-to-Peer", "Code + Peer-to-Peer"],
    OutputFile = output_file(Simulation),
    Result = run_gnuplot(InputFiles, Titles, OutputFile),
    io:format("Generating average plot ~p. Output: ~p~n~n", [OutputFile, Result]),

    delete_files(InputFiles).

%% @private
average(Simulation, EvalIds) ->
    lists:foldl(
        fun(EvalId, Map) ->
            PartitionProbability = get_partition_probability(EvalId),
            EvalIdDir = root_log_dir() ++ "/" ++ Simulation ++ "/" ++ EvalId,
            EvalTimestamps = only_dirs(EvalIdDir),
						Avg = average_executions(EvalIdDir, EvalTimestamps),
            orddict:store(PartitionProbability, Avg, Map)
        end,
        orddict:new(),
        EvalIds
    ).

%% @private
get_partition_probability(EvalId) ->
    Tokens = string:tokens(EvalId, "_"),
    PartitionProbability = lists:last(Tokens),
    list_to_integer(PartitionProbability).

%% @private
average_executions(EvalIdDir, EvalTimestamps) ->
    Sum = lists:foldl(
        fun(EvalTimestamp, Acc) ->
            File = EvalIdDir ++ "/" ++ EvalTimestamp ++ "/divergence",
            Divergence = get_divergence(File),
            Acc + Divergence
        end,
        0,
        EvalTimestamps
    ),

    Sum / length(EvalTimestamps).

%% @private
get_divergence(FilePath) ->
    %% Open log file
    {ok, FileDescriptor} = file:open(FilePath, [read]),
    Line = io:get_line(FileDescriptor, ''),
    list_to_float(Line).

%% @private
write_to_file(Simulation, Name, Map) ->
    Path = root_plot_dir() ++ "/" ++ Simulation ++ "/",
    filelib:ensure_dir(Path),
    InputFile = Path ++ Name ++ ".csv",
    file:write_file(InputFile, "", [write]),

    lists:foreach(
        fun({PartitionProbability, Divergence}) ->
            append_to_file(InputFile, PartitionProbability, Divergence)
        end,
        Map
    ),

    InputFile.

%% @private
append_to_file(InputFile, PartitionProbability, Divergence) ->
    Line = io_lib:format("~w,~w\n", [PartitionProbability, Divergence]),
    file:write_file(InputFile, Line, [append]).

%% @private
root_log_dir() ->
    "logs".

%% @private
root_plot_dir() ->
    "plots".

%% @private
gnuplot_file() ->
    "partition_divergence.gnuplot".

%% @private
output_file(Simulation) ->
    root_plot_dir() ++ "/" ++ Simulation ++ "/partition_divergence.pdf".

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
            % "partition_divergence.gnuplot" does not support titles with spaces
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
