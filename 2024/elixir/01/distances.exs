defmodule Distances do
  import Enum

  defp read_file do
    File.stream!("input.txt")
    |> Stream.map(&String.trim_trailing(&1, "\n"))
  end

  defp parse_pair(line) do
    String.split(line)
    |> map(&(Integer.parse(&1) |> elem(0)))
  end

  defp parse_input do
    read_file()
    |> map(&parse_pair/1)
  end

  defp do_split_lists([], {lhs, rhs}), do: {lhs, rhs}

  defp do_split_lists([[a, b] | tail], {lhs, rhs}) do
    do_split_lists(tail, {[a | lhs], [b | rhs]})
  end

  defp split_lists(input) do
    do_split_lists(input, {[], []})
  end

  defp distances({lhs, rhs}) do
    sum(map(zip(sort(lhs), sort(rhs)), fn {a, b} -> abs(a - b) end))
  end

  defp do_freq([], acc), do: acc

  defp do_freq([x | xs], m) do
    do_freq(xs, Map.put(m, x, Map.get(m, x, 0) + 1))
  end

  defp freq(list) do
    do_freq(list, %{})
  end

  defp do_score([], _, acc) do
    acc
  end

  defp do_score([x | xs], fre, acc) do
    s = Map.get(fre, x, 0)
    do_score(xs, fre, acc + s * x)
  end

  defp score(lhs, rhs) do
    do_score(lhs, freq(rhs), 0)
  end

  def run do
    lists = {lhs, rhs} = parse_input() |> split_lists()
    part_1 = lists |> distances()
    part_2 = score(lhs, rhs)
    IO.puts("part 1: #{part_1}")
    IO.puts("part 2: #{inspect(part_2)}")
  end
end

Distances.run()
