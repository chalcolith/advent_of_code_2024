use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        (let after, let updates) = read_data(arg)?
        step_1(after, updates)
        step_2(after, updates)
      end
    end

  fun read_data(fname: String)
    : (Map[USize, Set[USize]], Array[Array[USize]]) ?
  =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      let after = Map[USize, Set[USize]]
      let updates = Array[Array[USize]]

      for line in FileLines(file) do
        let rule_tokens = line.split_by("|")
        if rule_tokens.size() == 2 then
          _read_rule(consume rule_tokens, after)
        else
          let update_tokens = line.split_by(",")
          _read_update(consume update_tokens, updates)
        end
      end

      (after, updates)
    else
      _env.err.print("Unable to open " + fname)
      error
    end

  fun step_1(
    after: Map[USize, Set[USize]],
    updates: Array[Array[USize]])
  =>
    var sum_update_middles: USize = 0
    for row in updates.values() do
      if is_correct(after, row) then
        try
          let middle = row(row.size() / 2)?
          sum_update_middles = sum_update_middles + middle
        end
      end
    end
    _env.out.print("Step 1: " + sum_update_middles.string())

  fun step_2(after: Map[USize, Set[USize]], updates: Array[Array[USize]]) =>
    let empty_set = Set[USize]
    var sum_middles: USize = 0
    for row in updates.values() do
      if not is_correct(after, row) then
        try
          // good old insertion sort
          let ordered = Array[USize](row.size())
          ordered.push(row(0)?)
          for i in Range(1, row.size()) do
            let n = row(i)?
            var inserted = false
            for j in Range(0, ordered.size()) do
              let m = ordered(j)?
              if after.get_or_else(n, empty_set).contains(m) then
                ordered.insert(j, n)?
                inserted = true
                break
              end
            end
            if not inserted then
              ordered.push(n)
            end
          end
          let middle = ordered(ordered.size() / 2)?
          sum_middles = sum_middles + middle
        end
      end
    end
    _env.out.print("Step 2: " + sum_middles.string())

  fun is_correct(
    after: Map[USize, Set[USize]],
    row: Array[USize])
    : Bool
  =>
    try
      for i in Range(0, row.size() - 1) do
        for j in Range(i + 1, row.size()) do
          if not after(row(i)?)?.contains(row(j)?) then
            return false
          end
        end
      end
      true
    else
      false
    end

  fun _read_rule(
    rule_tokens: Array[String] iso, after: Map[USize, Set[USize]])
  =>
    try
      let first = rule_tokens(0)?.usize()?
      let next = rule_tokens(1)?.usize()?

      let a =
        match try after(first)? end
        | let a': Set[USize] =>
          a'
        else
          let a' = Set[USize]
          after.insert(first, a')
          a'
        end
      a.set(next)
    end

  fun _read_update(
    update_tokens: Array[String] iso,
    updates: Array[Array[USize]])
  =>
    if update_tokens.size() > 1 then
      updates.push(
        Iter[String]((consume update_tokens).values())
          .filter_map[USize]({(token) => try token.usize()? end})
          .collect[Array[USize]]([]))
    end
