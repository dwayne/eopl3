module Thread (Thread, new, run) where


data Thread a
  = Thread (() -> a)


new :: (() -> a) -> Thread a
new = Thread


run :: Thread a -> a
run (Thread f) = f ()
