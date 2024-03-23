import gleam/io
import simplifile
import gleam/result
import gleam/string
import gleam/list
import gleam/int

pub type Race {
  Race(time: Int, dist: Int)
}

fn parse_line(line: String) -> List(Int) {
  line
  |> string.split(" ")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.drop(1)
  |> list.map(int.parse)
  |> list.map(result.unwrap(_, -1))
}

fn parse_input(in: String) -> List(Race) {
  let split = in
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })

  let time = split
  |> list.first
  |> result.unwrap("")
  |> parse_line

  let dist = split
  |> list.last
  |> result.unwrap("")
  |> parse_line

  let same_len = list.length(time) == list.length(dist)
  case same_len {
    True -> {
      list.zip(time, dist)
      |> list.map(fn(x) { Race(time: x.0, dist: x.1) })
    }
    False -> []
  }
}

fn get_ways_to_win(race: Race) -> Int {
  get_ways_to_win_helper(race, 0, 0)
}

fn get_ways_to_win_helper(race: Race, t: Int, count: Int) -> Int {
  case race.time == t {
    True -> count
    False -> {
      let dt = race.time - t
      let distance = dt * t

      case distance {
        d if d > race.dist -> get_ways_to_win_helper(race, t + 1, count + 1)
        _ -> get_ways_to_win_helper(race, t + 1, count)
      }
    }
  }
}

fn combine_races(races: List(Race)) -> Race {
  let time = races
  |> list.map(fn(x) { x.time })
  |> list.map(int.to_string)
  |> string.join("")
  |> int.parse
  |> result.unwrap(-1)

  let dist = races
  |> list.map(fn(x) { x.dist })
  |> list.map(int.to_string)
  |> string.join("")
  |> int.parse
  |> result.unwrap(-1)

  Race(time: time, dist: dist)
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> parse_input

  let p1 = input
  |> list.map(get_ways_to_win)
  |> list.reduce(fn(acc, x) { acc * x })
  |> result.unwrap(-1)
  |> int.to_string

  io.println("-- Part 1 --")
  io.println(p1)

  let p2 = input
  |> combine_races
  |> get_ways_to_win
  |> int.to_string

  io.println("-- Part 2 --")
  io.println(p2)
}
