defmodule Factorial do
  @moduledoc false


  def factorial n do
    if n == 0, do: 1, else: n * factorial n-1
  end
end
