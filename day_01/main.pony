use "collections"
use "files"
use "itertools"

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        (let list_a, let list_b) = read_lists(arg)?
        step_1(list_a, list_b)
        step_2(list_a, list_b)
      end
    end

  fun read_lists(fname: String): (Array[I32], Array[I32]) ? =>
    let lines =
      match OpenFile(FilePath(FileAuth(_env.root), fname))
      | let file: File =>
        FileLines(file)
      else
        _env.err.print("Unable to open " + fname)
        error
      end

    Iter[String iso^](lines)
      .filter_map[(I32, I32)](
        {(line) =>
          try
            let tokens = line.split_by("   ")
            (tokens(0)?.i32()?, tokens(1)?.i32()?)
          end
        })
      .fold[(Array[I32], Array[I32])](
        ([], []),
        {(acc, next) =>
          (let a, let b) = acc
          (let i, let j) = next
          (a .> push(i), b .> push(j))
        })

  fun step_1(a: Array[I32], b: Array[I32]) =>
    let sort = Sort[Array[I32], I32]
    let s_a = sort(a)
    let s_b = sort(b)

    var sum: I32 = 0
    for (i, n_a) in s_a.pairs() do
      try
        sum = sum + I32.from[U32]((n_a - s_b(i)?).abs())
      end
    end

    _env.out.print("Step 1: " + sum.string())

  fun step_2(a: Array[I32], b: Array[I32]) =>
    let counts = Map[I32, I32]
    for n in b.values() do
      counts.upsert(n, 1, {(old, cur) => old + cur})
    end
    var sum: I32 = 0
    for n in a.values() do
      try
        sum = sum + (n * counts(n)?)
      end
    end

    _env.out.print("Step 2: " + sum.string())
