import gleam/io
import simplifile
import gleam/string
import gleam/result
import gleam/list
import gleam/int

pub type Card {
  Card(number: Int, winners: List(Int), selected: List(Int), n_instances: Int, score: Result(Int, Nil))
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

  Card(n, winners, selected, 1, Error(Nil))
}

fn score_card(card: Card) -> Int {
  score_card_helper(card.winners, card.selected, 0)
}

fn score_card_helper(winners: List(Int), selected: List(Int), res: Int) -> Int {
  let first_in_winner = selected
  |> list.first
  |> result.map(list.contains(winners, _))
  |> result.unwrap(False)

  case selected, res, first_in_winner {
    [], r, _ -> r
    [_, ..rest], 0, True -> score_card_helper(winners, rest, 1)
    [_, ..rest], r, True -> score_card_helper(winners, rest, r * 2)
    [_, ..rest], r, _ -> score_card_helper(winners, rest, r)
  }
}

fn pt2_score_card(card: Card) -> Int {
  pt2_score_card_helper(card.winners, card.selected, 0)
}

fn pt2_score_card_helper(winners: List(Int), selected: List(Int), score: Int) -> Int {
  let first_in_winners = selected
  |> list.first
  |> result.map(fn(x) { list.contains(winners, x) })
  |> result.unwrap(False)

  case selected, first_in_winners {
    [], _ -> score
    [_, ..rest], True -> pt2_score_card_helper(winners, rest, score + 1)
    [_, ..rest], False -> pt2_score_card_helper(winners, rest, score)
  }
}

fn pt2_count_cards(cards: List(Card)) -> Int {
  pt2_count_cards_helper(cards, 0)
}

fn pt2_count_cards_helper(cards: List(Card), res: Int) -> Int {
  case cards {
    [] -> res
    [x, ..rest] -> {
      let score = case x.score {
        Ok(s) -> s
        _ -> pt2_score_card(x)
      }

      let #(to_duplicate, rest) = rest
      |> list.split(score)

      let duplicated = to_duplicate
      |> list.map(fn(x) { Card(..x, n_instances: x.n_instances + 1) })

      let rest = list.append(duplicated, rest)

      let cards_left = x.n_instances - 1
      case cards_left {
        n if n > 0 -> {
          let updated_x = Card(..x, n_instances: cards_left)
          pt2_count_cards_helper(list.append([updated_x], rest), res + 1)
        }
        _ -> {
          pt2_count_cards_helper(rest, res + 1)
        }
      }
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

  let p2 = cards
  |> pt2_count_cards
  |> int.to_string

  io.println("-- Part 2 --")
  io.println("Number of cards: " <> p2)
}
