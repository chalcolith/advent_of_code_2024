use "collections"
use "debug"
use "files"
use "itertools"

type BlockRecord is (USize, USize, USize)

actor Main
  let _env: Env

  new create(env: Env) =>
    _env = env

    for arg in Iter[String](env.args.values()).skip(1) do
      try
        let blocks = read_data(arg)?
        step_1(blocks.clone())
        step_2(blocks.clone())
      end
    end

  fun step_1(blocks: Array[BlockRecord]) =>
    defrag_1(blocks)
    consolidate(blocks)
    //_print_blocks(blocks)
    let sum = checksum(blocks)
    _env.out.print("Part 1: " + sum.string())

  fun step_2(blocks: Array[BlockRecord]) =>
    //_print_blocks(blocks)
    defrag_2(blocks)
    consolidate(blocks)
    let sum = checksum(blocks)
    _env.out.print("Part 2: " + sum.string())

  fun checksum(blocks: Array[BlockRecord]): USize =>
    var sum: USize = 0
    for (id, idx, len) in blocks.values() do
      for j in Range[USize](0, len) do
        sum = sum + (id * (idx + j))
      end
    end
    sum

  fun defrag_1(blocks: Array[BlockRecord]) =>
    (var first_gap, var done) = _find_first_gap(blocks, 0, 1)
    while not done do
      try
        let last_index = blocks.size() - 1
        let last_block = blocks(last_index)?
        let preceeding = blocks(first_gap)?

        if last_block._3 == 1 then
          blocks.delete(last_index)?
        else
          blocks(last_index)? =
            (last_block._1, last_block._2, last_block._3 - 1)
        end

        if (preceeding._1 == last_block._1) then
          blocks(first_gap)? = (preceeding._1, preceeding._2, preceeding._3 + 1)
        else
          blocks.insert(
            first_gap + 1, (last_block._1, preceeding._2 + preceeding._3, 1))?
        end
      end
      (first_gap, done) = _find_first_gap(blocks, first_gap, 1)
    end

  fun defrag_2(blocks: Array[BlockRecord]) =>
    try
      var cur_id = blocks(blocks.size() - 1)?._1

      while cur_id > 0 do
        var i: USize = blocks.size() - 1
        while blocks(i)?._1 != cur_id do
          if i > 1 then
            i = i - 1
          else
            return
          end
        end

        let block_to_move = blocks(i)?
        (let first_gap, var done) = _find_first_gap(blocks, 0, block_to_move._3)
        if (not done) and (first_gap < i) then
          blocks.delete(i)?
          let preceeding = blocks(first_gap)?

          if preceeding._1 == block_to_move._1 then
            blocks(first_gap)? =
              (preceeding._1, preceeding._2, preceeding._3 + block_to_move._3)
          else
            blocks.insert(
              first_gap + 1,
              ( block_to_move._1
              , preceeding._2 + preceeding._3
              , block_to_move._3))?
          end
        end
        _print_blocks(blocks)

        cur_id = cur_id - 1
      end
    end

  fun consolidate(blocks: Array[BlockRecord]) =>
    var i: USize = 1
    while i < blocks.size() do
      try
        let first = blocks(i - 1)?
        let second = blocks(i)?
        if first._1 == second._1 then
          blocks.delete(i)?
          blocks(i - 1)? = (first._1, first._2, first._3 + second._3)
        else
          i = i + 1
        end
      end
    end

  fun _find_first_gap(
    blocks: Array[BlockRecord], start: USize, len: USize)
    : (USize, Bool)
  =>
    for i in Range(start + 1, blocks.size()) do
      try
        let first = blocks(i - 1)?
        let f_id = first._1
        let f_idx = first._2
        let f_len = first._3
        let second = blocks(i)?
        let s_id = second._1
        let s_idx = second._2
        let s_len = second._3

        if ((f_idx + f_len) < s_idx) and
          ((s_idx - (f_idx + f_len)) >= len)
        then
          return (i - 1, false)
        end
      end
    end
    (USize.max_value(), true)

  fun _print_blocks(blocks: Array[BlockRecord]) =>
    let str: String trn = String
    var cur_idx: USize = 0
    for (id, idx, len) in blocks.values() do
      while cur_idx < idx do
        str.append(".")
        cur_idx = cur_idx + 1
      end
      for i in Range[USize](0, len) do
        str.append((id % 10).string())
      end
      cur_idx = cur_idx + len
    end
    Debug.out(consume str)
    Debug.out("")

  fun read_data(fname: String): Array[BlockRecord] ? =>
    match OpenFile(FilePath(FileAuth(_env.root), fname))
    | let file: File =>
      let map = file.read_string(file.size())

      let result = Array[BlockRecord] // id, index, len
      var block_id: USize = 0
      var file_index: USize = 0
      var i: USize = 0
      for ch in (consume map).values() do
        if not ((ch >= '0') and (ch <= '9')) then continue end

        let len = (ch - '0').usize()
        if (i % 2) == 0 then
          result.push((block_id, file_index, len))
          block_id = block_id + 1
        end
        file_index = file_index + len
        i = i + 1
      end
      result
    else
      _env.err.print("Unable to open " + fname)
      error
    end
