use "collections"
use "files"
use "itertools"

primitive Add
primitive Mul
primitive Cat

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let rows = read_data(arg)?
        step_1(rows)
        step_2(rows)
      end
    end

  fun step_1(rows: Array[Array[I64]]) =>
    var num_possible: I64 = 0

    for row in rows.values() do
      try
        if
          _found_ops(row, row(1)?, 2, Add) or _found_ops(row, row(1)?, 2, Mul)
        then
          num_possible = num_possible + row(0)?
        end
      end
    end

    _env.out.print("Step 1: " + num_possible.string())

  fun step_2(rows: Array[Array[I64]]) =>
    var num_possible: I64 = 0

    for row in rows.values() do
      try
        if
          _found_ops(row, row(1)?, 2, Add, true) or
          _found_ops(row, row(1)?, 2, Mul, true) or
          _found_ops(row, row(1)?, 2, Cat, true)
        then
          num_possible = num_possible + row(0)?
        end
      end
    end

    _env.out.print("Step 1: " + num_possible.string())

  fun _found_ops(
    row: Array[I64],
    acc: I64,
    pos: USize,
    op: (Add | Mul | Cat),
    use_cat: Bool = false)
    : Bool
  =>
    try
      if pos == row.size() then
        return row(0)? == acc
      end

      let acc' =
        match op
        | Add => acc + row(pos)?
        | Mul => acc * row(pos)?
        | Cat => (acc.string() + row(pos)?.string()).i64()?
        end

      _found_ops(row, acc', pos + 1, Add, use_cat) or
      _found_ops(row, acc', pos + 1, Mul, use_cat) or
      (use_cat and _found_ops(row, acc', pos + 1, Cat, use_cat))
    else
      false
    end

  fun read_data(fname: String): Array[Array[I64]] ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      Iter[String iso^](FileLines(file))
        .filter_map[Array[I64]](
          {(line) =>
            try
              let tokens1 = line.split_by(": ")
              if tokens1.size() == 2 then
                return Iter[String](tokens1(1)?.split_by(" ").values())
                  .filter_map[I64]({(s) => try s.i64()? end })
                  .collect[Array[I64]]([ tokens1(0)?.i64()? ])
              end
            end
          })
        .collect(Array[Array[I64]])
    else
      _env.err.print("Unable to open " + fname)
      error
    end
