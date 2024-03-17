import gleam/io
import simplifile
import gleam/result
import gleam/string
import gleam/list
import gleam/int

const n_reds = 12
const n_greens = 13
const n_blues = 14

fn get_game_id(game: String) -> Result(Int, Nil) {
  let start = game
  |> string.split(":")
  |> list.first

  let string_id = case start {
    Ok("Game " <> id) -> Ok(id)
    _ -> Error(Nil)
  }

  case string_id {
    Ok(x) -> int.parse(x)
    _ -> Error(Nil)
  }
}

fn is_legal(s: String) -> Bool {
  let split = s
  |> string.split(" ")

  let number = split
  |> list.first
  |> result.map(int.parse)
  |> result.flatten

  let color = split
  |> list.last

  case number, color {
    Ok(x), Ok("green") -> x <= n_greens
    Ok(x), Ok("red") -> x <= n_reds
    Ok(x), Ok("blue") -> x <= n_blues
    _, _ -> False
  }
}

fn is_legal_subgame(subgame: String) -> Bool {
  subgame
  |> string.split(", ")
  |> list.all(is_legal)
}

fn is_legal_game(game: String) -> Bool {
  let all_legal = game
  |> string.split(": ")
  |> list.last
  |> result.map(string.split(_, "; "))
  |> result.map(list.all(_, fn(x) { is_legal_subgame(x) }))

  case all_legal {
    Ok(x) -> x
    _ -> False
  }
}

fn add_game(game: String) -> Int {
  let id = game
  |> get_game_id

  let is_legal = game
  |> is_legal_game

  case id, is_legal {
    Ok(x), True -> x
    _, _ -> 0
  }
}

fn to_ints(lst: List(String)) -> List(Result(Int, Nil)) {
  lst
  |> list.map(fn(x) { string.split(x, " ") })
  |> list.map(list.first)
  |> list.map(fn(x) { result.map(x, int.parse) })
  |> list.map(result.flatten)
}

fn get_max(lst: List(Result(Int, Nil))) -> Result(Int, Nil) {
  case lst {
    [] -> Error(Nil)
    [Ok(x)] -> Ok(x)
    [Error(_x)] -> Error(Nil)
    [Ok(x), ..rest] -> Ok(get_max_helper(rest, x))
    [Error(_x), ..rest] -> get_max(rest)
  }

}

fn get_max_helper(lst: List(Result(Int, Nil)), curr_max: Int) -> Int {
  case lst {
    [] -> curr_max
    [Error(_x)] -> curr_max
    [Ok(x)] if x > curr_max -> x
    [Ok(x)] if x <= curr_max -> curr_max
    [Ok(x), ..rest] if x > curr_max -> get_max_helper(rest, x)
    [Ok(_x), ..rest] -> get_max_helper(rest, curr_max)
    [Error(_x), ..rest] -> get_max_helper(rest, curr_max)
  }
}

fn get_power(game: String) -> Result(Int, Nil) {
  let cubes = game
  |> string.split(": ")
  |> list.last
  |> result.map(string.split(_, "; "))
  |> result.map(list.map(_, fn(x) { string.split(x, ", ") }))
  |> result.map(list.flatten(_))

  let green_cubes = cubes
  |> result.map(list.filter(_, fn(x) { string.contains(x, "green") }))
  |> result.map(to_ints)
  |> result.map(get_max)
  |> result.flatten

  let blue_cubes = cubes
  |> result.map(list.filter(_, fn(x) { string.contains(x, "blue") }))
  |> result.map(to_ints)
  |> result.map(get_max)
  |> result.flatten

  let red_cubes = cubes
  |> result.map(list.filter(_, fn(x) { string.contains(x, "red") }))
  |> result.map(to_ints)
  |> result.map(get_max)
  |> result.flatten

  case green_cubes, blue_cubes, red_cubes {
    Ok(x), Ok(y), Ok(z) -> Ok(x * y * z)
    _, _, _ -> Error(Nil)
  }
}

pub fn main() {
  let input = "input.txt"
  |> simplifile.read
  |> result.map(string.split(_, "\n"))
  |> result.map(list.filter(_, fn(x) { x != "" }))

  let sum = input
  |> result.map(list.fold(_, 0, fn(acc, game) { add_game(game) + acc }))

  io.print("Part 1: ")
  case sum {
    Ok(x) -> io.println(int.to_string(x))
    _ -> io.println("Error")
  }

  let power_sum = input
  |> result.map(list.fold(_, 0, fn(acc, game) {
    result.unwrap(get_power(game), 0) + acc
  }))

  io.print("Part 2: ")
  case power_sum {
    Ok(x) -> io.println(int.to_string(x))
    _ -> io.println("Error")
  }
}
