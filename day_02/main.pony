use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let data = read_data(arg)?
        step(1, data, false)
        step(2, data, true)
      end
    end

  fun read_data(fname: String): Array[Array[I32]] ? =>
    let lines =
      match OpenFile(FilePath(FileAuth(_env.root), fname))
      | let file: File =>
        FileLines(file)
      else
        _env.err.print("Unable to open " + fname)
        error
      end

    Iter[String iso^](lines)
      .map[Array[I32]](
        {(line) =>
          Iter[String val](line.split_by(" ").values())
            .filter_map[I32]({(token) => try token.i32()? end})
            .collect(Array[I32])
        })
      .collect(Array[Array[I32]])

  fun step(num: USize, data: Array[Array[I32]], tolerate: Bool) =>
    var num_safe: USize = 0
    for row in data.values() do
      for r in Range(0, row.size()) do
        let safe =
          try
            Iter[I32](row.values())
              .fold_partial[(USize, I32, I32, I32)](
                (0, 0, 0, 0), // i, i', prev, sign
                {(acc: (USize, I32, I32, I32), n: I32)
                  : (USize, I32, I32, I32) ?
                =>
                  (let i, let i', let prev, let sign) = acc
                  if tolerate and (i == r) then
                    (i + 1, i', prev, sign)
                  elseif i' == 0 then
                    (1, 1, n, 0)
                  elseif
                    (n == prev) or
                    ((n - prev).abs() > 3) or
                    ((i' > 1) and (((n - prev) < 0) != (sign < 0)))
                  then
                    error
                  else
                    (i + 1, i' + 1, n, n - prev)
                  end
                })?
            true
          else
            false
          end
        if safe then
          num_safe = num_safe + 1
          break
        end
        if not tolerate then
          break
        end
      end
    end
    _env.out.print("Step " + num.string() + ": " + num_safe.string())
