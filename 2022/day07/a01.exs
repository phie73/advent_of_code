defmodule FileSystem do
  def buildTree(lines) do
    buildTree(%{}, lines, nil)
  end

  def buildTree(fileSystem, [], _) do
    fileSystem
  end

  def buildTree(fileSystem, [line | tail], pwd) do
    case line do
      "$ cd /" ->
        new_pwd = "/"
        buildTree(fileSystem, tail, new_pwd)

      "$ cd " <> dir ->
        new_pwd = Path.expand(dir, pwd)
        buildTree(fileSystem, tail, new_pwd)

      "$ ls" ->
        buildTree(fileSystem, tail, pwd)

      "dir " <> dir ->
        new_fs = addToDir(fileSystem, pwd, {"d", dir})
        buildTree(new_fs, tail, pwd)

      _ ->
        [size_s, _] = String.split(line, " ")
        size = String.to_integer(size_s)
        new_fs = addToDir(fileSystem, pwd, {"f", size})
        buildTree(new_fs, tail, pwd)
    end
  end

  def getDir(fileSystem, path) do
    Map.get(fileSystem, path, [])
  end

  def addToDir(fileSystem, path, {type, value}) do
    old_value = getDir(fileSystem, path)
    new_value = [{type, value} | old_value]
    Map.put(fileSystem, path, new_value)
  end

  def calculateSize(fileSystem, dir) do
    getDir(fileSystem, dir)
    |> Enum.map(fn {type, value} ->
      if type == "d" do
        calculateSize(fileSystem, Path.expand(value, dir))
      else
        value
      end
    end)
    |> Enum.sum()
  end
end

defmodule Main do
  @totalSpace 70_000_000
  @required 30_000_000

  def part1(fileSystem) do
    fileSystem
    |> Enum.map(fn {dir, _} -> FileSystem.calculateSize(fileSystem, dir) end)
    |> Enum.filter(fn size -> size <= 100_000 end)
    |> Enum.sum()
  end

  def part2(fileSystem) do
    freeSpace = @totalSpace - FileSystem.calculateSize(fileSystem, "/")
    reqSpace = @required - freeSpace

    fileSystem
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&hd/1)
    |> Enum.map(fn dir -> FileSystem.calculateSize(fileSystem, dir) end)
    |> Enum.filter(fn size -> size >= reqSpace end)
    |> Enum.min()
  end

  def solve(filename) do
    lines = File.read!(filename) |> String.split("\n", trim: true)
    fileSystem = FileSystem.buildTree(lines)
    part1(fileSystem) |> IO.inspect()
    part2(fileSystem) |> IO.inspect()
  end
end

Main.solve("input.txt")
