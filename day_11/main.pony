use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env
  let _stones: Map[U128, USize]

  new create(env: Env) =>
    _env = env
    _stones = _stones.create()

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        read_data(arg)?
        blink(0)
      end
    end

  be blink(n: USize) =>
    // we use a behaviour here so we can garbage-collect the temp strings
    if ((n % 5) == 0) then
      _env.out.print(n.string())
    end

    if n == 75 then
      var num: USize = 0
      for count in _stones.values() do
        num = num + count
      end
      _env.out.print("Part 2: " + num.string())
      return
    end

    if n == 25 then
      var num: USize = 0
      for count in _stones.values() do
        num = num + count
      end
      _env.out.print("Part 1: " + num.string())
    end

    let to_insert = Array[(U128, USize)]
    for (stone, count) in _stones.pairs() do
      if stone == 0 then
        to_insert.push((1, count))
      else
        let str = stone.string()
        if ((str.size() % 2) == 0) then
          (let a, let b) = (consume str).chop(str.size() / 2)
          try
            let a' = a.u128()?
            let b' = b.u128()?
            if a' == b' then
              to_insert.push((a', count * 2))
            else
              to_insert.push((a', count))
              to_insert.push((b', count))
            end
          end
        else
          to_insert.push(((stone * 2024), count))
        end
      end
    end
    _stones.clear()
    for (stone, count) in to_insert.values() do
      _stones.upsert(stone, count, {(a, b) => a + b})
    end

    blink(n + 1)

  fun ref read_data(fname: String) ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      _stones.clear()
      for token in file.read_string(file.size()).split_by(" ").values() do
        try
          let n = token.u128()?
          _stones.upsert(n, 1, {(a, b) => a + b})
        end
      end
    else
      _env.err.print("Unable to open " + fname)
      error
    end
