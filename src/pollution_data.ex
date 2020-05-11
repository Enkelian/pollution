defmodule PollutionData do
    @moduledoc false


    def importLinesFromCSV do
      lines = File.read!("pollution.csv") |> String.split("\n")
      Enum.map(lines, fn line -> parseCSVLine line end)
    end

    def loadRecords recordsList do
      Enum.map(recordsList, fn record -> addRecordToServer record end)
    end

    def loadStations recordsList do
      stationsList = identifyStations recordsList
      Enum.map(stationsList, fn station -> addStationToServer station end)
    end

    def parseCSVLine line do
      [date, time, length, width, value] = String.split(line, ",")
      dateTuple =
        date
        |> String.split("-")
        |> Enum.reverse() |> Enum.map(fn elem -> String.to_integer elem end) |> :erlang.list_to_tuple()
      timeTuple = time |> String.split(":") |> Enum.map(fn elem -> String.to_integer elem end) |> Kernel.++([0]) |> :erlang.list_to_tuple()
      {{length, _}, {width,_}} = {Float.parse(length), Float.parse(width)}
      {value, _} = Float.parse(value)
      %{:datetime => {dateTuple, timeTuple}, :location => {length, width}, :pollutionLevel => value}
    end

    def identifyStations recordsList do
      Enum.uniq_by(recordsList, fn record -> record.location end)
    end

    def addStationToServer station do
      stationName = "station_#{elem(station.location, 0)}_#{elem(station.location,1)}"
      :pollution_gen_server.addStation(to_charlist(stationName), station.location)
    end

    def addRecordToServer record do
      :pollution_gen_server.addValue(record.location, record.datetime, 'PM10', record.pollutionLevel)
    end

    def timeExecution fun, arg do
      :timer.tc(fun, [arg])  |> elem(0)
    end

  end