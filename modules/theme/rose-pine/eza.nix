p: ''
  colourful: true

  filekinds:
    normal: {foreground: "${p.text}"}
    directory: {foreground: "${p.iris}"}
    symlink: {foreground: "${p.foam}"}
    pipe: {foreground: "${p.subtle}"}
    block_device: {foreground: "${p.rose}"}
    char_device: {foreground: "${p.rose}"}
    socket: {foreground: "${p.subtle}"}
    special: {foreground: "${p.iris}"}
    executable: {foreground: "${p.pine}"}
    mount_point: {foreground: "${p.foam}"}

  perms:
    user_read: {foreground: "${p.love}", is_bold: true}
    user_write: {foreground: "${p.gold}", is_bold: true}
    user_execute_file: {foreground: "${p.pine}", is_bold: true}
    user_execute_other: {foreground: "${p.pine}", is_bold: true}
    group_read: {foreground: "${p.love}"}
    group_write: {foreground: "${p.gold}"}
    group_execute: {foreground: "${p.pine}"}
    other_read: {foreground: "${p.love}"}
    other_write: {foreground: "${p.gold}"}
    other_execute: {foreground: "${p.pine}"}
    special_user_file: {foreground: "${p.iris}"}
    special_other: {foreground: "${p.muted}"}
    attribute: {foreground: "${p.muted}"}

  size:
    major: {foreground: "${p.subtle}"}
    minor: {foreground: "${p.foam}"}
    number_byte: {foreground: "${p.subtle}"}
    number_kilo: {foreground: "${p.subtle}"}
    number_mega: {foreground: "${p.foam}"}
    number_giga: {foreground: "${p.iris}"}
    number_huge: {foreground: "${p.iris}"}
    unit_byte: {foreground: "${p.subtle}"}
    unit_kilo: {foreground: "${p.foam}"}
    unit_mega: {foreground: "${p.iris}"}
    unit_giga: {foreground: "${p.iris}"}
    unit_huge: {foreground: "${p.rose}"}

  users:
    user_you: {foreground: "${p.text}"}
    user_root: {foreground: "${p.love}"}
    user_other: {foreground: "${p.rose}"}
    group_yours: {foreground: "${p.subtle}"}
    group_other: {foreground: "${p.muted}"}
    group_root: {foreground: "${p.love}"}

  links:
    normal: {foreground: "${p.foam}"}
    multi_link_file: {foreground: "${p.foam}"}

  git:
    new: {foreground: "${p.pine}"}
    modified: {foreground: "${p.gold}"}
    deleted: {foreground: "${p.love}"}
    renamed: {foreground: "${p.foam}"}
    typechange: {foreground: "${p.iris}"}
    ignored: {foreground: "${p.muted}"}
    conflicted: {foreground: "${p.rose}"}

  git_repo:
    branch_main: {foreground: "${p.subtle}"}
    branch_other: {foreground: "${p.iris}"}
    git_clean: {foreground: "${p.pine}"}
    git_dirty: {foreground: "${p.love}"}

  security_context:
    colon: {foreground: "${p.muted}"}
    user: {foreground: "${p.subtle}"}
    role: {foreground: "${p.iris}"}
    typ: {foreground: "${p.highlightHigh}"}
    range: {foreground: "${p.iris}"}

  file_type:
    image: {foreground: "${p.gold}"}
    video: {foreground: "${p.love}"}
    music: {foreground: "${p.pine}"}
    lossless: {foreground: "${p.foam}"}
    crypto: {foreground: "${p.subtle}"}
    document: {foreground: "${p.text}"}
    compressed: {foreground: "${p.iris}"}
    temp: {foreground: "${p.rose}"}
    compiled: {foreground: "${p.foam}"}
    source: {foreground: "${p.foam}"}

  punctuation: {foreground: "${p.muted}"}
  date: {foreground: "${p.gold}"}
  inode: {foreground: "${p.subtle}"}
  blocks: {foreground: "${p.muted}"}
  header: {foreground: "${p.text}"}
  octal: {foreground: "${p.foam}"}
  flags: {foreground: "${p.iris}"}

  symlink_path: {foreground: "${p.foam}"}
  control_char: {foreground: "${p.foam}"}
  broken_symlink: {foreground: "${p.love}"}
  broken_path_overlay: {foreground: "${p.highlightHigh}"}
''
