import gleam/io
import simplifile
import gleam/string
import gleam/result
import gleam/list
import gleam/int

pub type Card {
  Card(number: Int, winners: List(Int), selected: List(Int))
}

fn parse_num_string(s: String) -> List(Int) {
  s
  |> string.split(" ")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(fn(x) {
    x
    |> int.parse
    |> result.unwrap(-1)
  })
}

fn parse_card(s: String) -> Card {
  let split = s
  |> string.split("|")

  let first = split
  |> list.first
  |> result.unwrap("")
  |> string.split(":")

  let n = first
  |> list.first
  |> result.unwrap("")
  |> string.split(" ")
  |> list.last
  |> result.unwrap("")
  |> int.parse
  |> result.unwrap(-1)

  let winners = first
  |> list.last
  |> result.unwrap("")
  |> string.trim
  |> parse_num_string

  let selected = split
  |> list.last
  |> result.unwrap("")
  |> string.trim
  |> parse_num_string

  Card(n, winners, selected)
}

fn score_card(card: Card) -> Int {
  score_card_helper(card.winners, card.selected, 0)
}

fn score_card_helper(winners: List(Int), selected: List(Int), res: Int) -> Int {
  let first_in_winner = selected
  |> list.first
  |> result.map(list.contains(winners, _))

  case selected, res, first_in_winner {
    [], r, _ -> r
    [_, ..rest], 0, Ok(True) -> score_card_helper(winners, rest, 1)
    [_, ..rest], r, Ok(True) -> score_card_helper(winners, rest, r * 2)
    [_, ..rest], r, _ -> score_card_helper(winners, rest, r)
  }
}

fn insert_cards(cards: List(Card), stack: List(Card)) -> List(Card) {
  case cards {
    [] -> stack
    [x, ..rest] -> insert_cards(rest, insert_card(x, stack))
  }
}

fn insert_card(card: Card, stack: List(Card)) -> List(Card) {
  let first_has_same_id = stack
  |> list.first
  |> result.map(fn(x) { x.number == card.number })
  |> result.unwrap(False)

  case stack, first_has_same_id {
    [], _ -> [card]
    s, True -> list.append([card], s)
    [x, ..rest], False -> {
      list.append([x], insert_card(card, rest))
    }
  }
}

fn remove_duplicates(lst: List(a)) -> List(a) {
  remove_duplicates_helper(lst, [])
}

fn remove_duplicates_helper(lst: List(a), res: List(a)) -> List(a) {
  case lst {
    [] -> res
    [x, ..rest] -> {
      case list.contains(res, x) {
        True -> remove_duplicates_helper(rest, res)
        False -> remove_duplicates_helper(rest, list.append(res, [x]))
      }
    }
  }
}

fn pt2_count_cards(cards: List(Card)) -> Int {
  pt2_count_cards_helper(cards, 0)
}

fn pt2_count_cards_helper(cards: List(Card), res: Int) -> Int {
  case cards {
    [] -> res - 1
    [x, ..rest] -> {
      let score = score_card(x)

      let #(new_cards, _) = rest
      |> remove_duplicates
      |> list.filter(fn(y) { y.number != x.number })
      |> list.split(score)

      let new_res = new_cards
      |> insert_cards(rest)

      pt2_count_cards_helper(new_res, res + 1)
    }
  }
}

pub fn main() {
  let cards = "input.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> string.split("\n")
  |> list.filter(fn(x) { !string.is_empty(x) })
  |> list.map(parse_card)

  let p1 = cards
  |> list.map(score_card)
  |> list.fold(0, fn(acc, x) { acc + x })
  |> int.to_string

  io.println("-- Part 1 --")
  io.println("Sum: " <> p1)

  //let p2 = cards
  //|> pt2_count_cards
  //|> int.to_string

  //io.println("-- Part 2 --")
  //io.println("Number of cards: " <> p2)
}
