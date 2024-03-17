import gleam/io
import simplifile
import gleam/result
import gleam/string
import gleam/list
import gleam/int

pub type GetLeftError {
  NoNumberError
}

fn get_left(s: String, reversed: Bool) -> Result(Int, GetLeftError) {
  case s, reversed {
    "0" <> _, _ -> Ok(0)
    "1" <> _, _ -> Ok(1)
    "2" <> _, _ -> Ok(2)
    "3" <> _, _ -> Ok(3)
    "4" <> _, _ -> Ok(4)
    "5" <> _, _ -> Ok(5)
    "6" <> _, _ -> Ok(6)
    "7" <> _, _ -> Ok(7)
    "8" <> _, _ -> Ok(8)
    "9" <> _, _ -> Ok(9)

    "one" <> _, False -> Ok(1)
    "two" <> _, False -> Ok(2)
    "three" <> _, False -> Ok(3)
    "four" <> _, False -> Ok(4)
    "five" <> _, False -> Ok(5)
    "six" <> _, False -> Ok(6)
    "seven" <> _, False -> Ok(7)
    "eight" <> _, False -> Ok(8)
    "nine" <> _, False -> Ok(9)

    "eno" <> _, True -> Ok(1)
    "owt" <> _, True -> Ok(2)
    "eerht" <> _, True -> Ok(3)
    "ruof" <> _, True -> Ok(4)
    "evif" <> _, True -> Ok(5)
    "xis" <> _, True -> Ok(6)
    "neves" <> _, True -> Ok(7)
    "thgie" <> _, True -> Ok(8)
    "enin" <> _, True -> Ok(9)

    "", _ -> Error(NoNumberError)
    _, _ -> get_left(string.drop_left(s, 1), reversed)
  }
}

fn get_number(s: String) -> Result(Int, GetLeftError) {
  let left = s
  |> get_left(False)

  let right = s
  |> string.reverse
  |> get_left(True)

  case left, right {
    Ok(left), Ok(right) -> Ok(left * 10 + right)
    _, _ -> Error(NoNumberError)
  }
}


pub fn main() {
  let result = "input.txt"
  |> simplifile.read
  |> result.map(string.split(_, "\n"))
  |> result.map(list.filter(_, fn(x) { x != "" }))
  |> result.map(list.map(_, get_number))
  |> result.map(list.fold(_, 0, fn(x, y) {
    case x, y {
      _, Ok(z) -> x + z
      _, _ -> x
    }
  }))

  case result {
    Ok(x) -> io.println(int.to_string(x))
    Error(_) -> io.println("Error when parsing strings")
  }
}
