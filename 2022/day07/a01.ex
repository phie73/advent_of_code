defmodule AOC do
#alias Inspect.Regex

    @type inputLine :: command() | filsystem_entry()

    @type command ::
            {:command, :ls}
            | {:command, :cd, :up}
            | {:command, :cd, :root}
            | {:command, :cd, String.t()}

    @type filsystem_entry() :: {:dir, String.t()} | {:file, String.t(), number()}
    @type fileSystem :: %{String.t() => fileSystem() | {:file, number()}}


    def unwrapInput({:ok, input}, _default) do
      input
    end

    def unwrapInput({:error, :no_impl}, default) do
        default
    end

    def readInput(file) do
        File.read!(file)
        |> String.trim_trailing()
        |> String.split("\n")
        |> Enum.map(&parseLine!/1)
    end

    defp parseLine!(line) do
      parsers = [
        &parseCommand/1,
        &parseDir/1,
        &parseFile/1
      ]
      parsed = Enum.find_value(
        parsers,
        fn parser ->
            case parser.(line) do
                {:ok, parsed} -> parsed
                {:error, :no_match} -> nil
            end
        end
      )
      if parsed == nil do
        raise ~s(Invalid line "#{line}")
      else
        parsed
      end
    end

    @spec parseCommand(String.t()) :: {:ok, command()} | {:error, :no_match}
    def parseCommand(line) do
      pattern = ~r/^\$ (ls|cd (.+))/
      case Regex.run(pattern, line) do
        [_match, "ls"] -> {:ok, {:command, :ls}}
        [_match, _cd, "/"] -> {:ok, {:command, :cd, :root}}
        [_match, _cd, ".."] -> {:ok, {:command, :cd, :up}}
        [_match, _cd, dirName] -> {:ok, {:command, :cd, dirName}}
        nil -> {:error, :no_match}
      end
    end


    @spec parseDir(String.t()) :: {:ok, {:dir, String.t()}} | {:error, :no_match}
    def parseDir(line) do
      pattern = ~r/^dir (.+)/
      case Regex.run(pattern, line) do
        [_match, name] -> {:ok, {:dir, name}}
        nil -> {:error, :no_match}
      end
    end

    @spec parseFile(String.t()) :: {:ok, {:file, String.t(), number()}} | {:error, :no_match}
    def parseFile(line) do
      pattern = ~r/^(\d+) (.+)/
      case Regex.run(pattern, line) do
        [_match, size, name] -> {:ok, {:file, name, String.to_integer(size)}}
        nil -> {:error, :no_match}
      end
    end

    @spec buildTree([inputLine()]) :: fileSystem()
    def buildTree([firstLine | lines]) do
      {:command, :cd, :root} = firstLine
      buildTree(lines, ["/"], %{"/" => %{}})
    end

    @spec buildTree([inputLine()], [String.t()], fileSystem()) :: fileSystem()
    def buildTree([], _, fileSystem) do
      fileSystem
    end

    def buildTree([inputLine | lines], path, fileSystem) do
      pwd = get_in(fileSystem, path)
      case inputLine do
        {:command, :ls} -> {dirEntries, afterLsLines} = getDirEntries(lines)

        updatePwd = Enum.map(dirEntries, fn
            {:dir, name} -> {name, %{}}
            {:file, name, size} -> {name, {:file, size}}
        end)
        |> Enum.into(pwd)

        updateFileSystem = put_in(fileSystem, path, updatePwd)
        buildTree(afterLsLines, path, updateFileSystem)

        {:command, :cd, :up} -> buildTree(lines, Enum.drop(path, -1), fileSystem)
        {:command, :cd, :root} -> buildTree(lines, ["/"], fileSystem)
        {:command, :cd, name} -> buildTree(lines, path ++ [name], fileSystem)
      end
    end

    @spec getDirEntries([inputLine()]) :: {[filsystem_entry()], [inputLine()]}
    def getDirEntries(Lines) do
      Lines |> Enum.split_while( fn
        {:file, _,_} -> true
        {:dir, _} -> true
        _ -> false
      end)
    end

    @spec getDirPaths(fileSystem) :: [[String.t()]]
    def getDirPaths(fileSystem) do
      getDirPaths(fileSystem, ["/"])
    end

    def getDirPaths({:file, _}, _) do
      []
    end

    def getDirPaths(fileSystem, cursor) do
      localDirs = fileSystem |>get_in(cursor)
                                |> Enum.reduce([], fn
                                    {_name, {:file, _}}, dirs -> dirs
                                    {name, %{}}, dirs -> [cursor ++ [name] | dirs]
                                end)
        Enum.reduce(localDirs, localDirs, fn dir, allDirs -> getDirPaths(fileSystem, dir) ++ allDirs end)
    end

    def fileSize({:file, size}) do
      size
    end

    def fileSize(fileSystem) do
      fileSystem |> Enum.map(fn {_, entry} -> fileSize(entry) end)
                    |> Enum.sum()
    end

    @spec a01(fileSystem()) :: number()
    def a01(fileSystem) do
      res = getDirPaths(fileSystem)
      |> Enum.map(&(get_in(fileSystem, &1) |> fileSize()))
      |> Enum.filter(&(&1 < 100_000))
      |> Enum.sum()

      IO.puts(res)
    end
end

IO.puts("hello world")

file = "input.txt"

input = AOC.readInput(file) |> AOC.unwrapInput(file)
# IO.puts(input)
# AOC.a01(input)

# output1 = apply(AOC, :a01, [input])
