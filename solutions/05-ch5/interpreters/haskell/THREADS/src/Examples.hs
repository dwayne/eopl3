module Examples
  ( example1
  , example2
  ) where


import qualified Interpreter as I


-- Example 1: Two threads showing interleaved computation.
--
-- NOTE: You can reproduce the output shown in the book
--       by using maxTimeSlice = 10.
example1 :: Int -> IO I.Value
example1 maxTimeSlice =
  let
    input =
      "letrec                                          \
      \  noisy(l) =                                    \
      \    if null?(l) then                            \
      \      0                                         \
      \    else                                        \
      \      begin                                     \
      \        print(car(l));                          \
      \        (noisy cdr(l))                          \
      \      end                                       \
      \in                                              \
      \begin                                           \
      \  spawn(proc (d) (noisy list(1, 2, 3, 4, 5)));  \
      \  spawn(proc (d) (noisy list(6, 7, 8, 9, 10))); \
      \  print(100);                                   \
      \  33                                            \
      \end                                             "
  in
  I.runIO maxTimeSlice input


-- Example 2: A producer and consumer, linked by a buffer.
--
example2 :: Int -> IO I.Value
example2 maxTimeSlice =
  let
    input =
      "let                                    \
      \  buffer = 0                           \
      \in                                     \
      \let                                    \
      \  producer =                           \
      \    proc (n)                           \
      \      letrec                           \
      \        wait(k) =                      \
      \          if zero?(k) then             \
      \            set buffer = n             \
      \          else                         \
      \            begin                      \
      \              print(-(k, -(0, 200)));  \
      \              (wait -(k, 1))           \
      \            end                        \
      \      in                               \
      \      (wait 5)                         \
      \in                                     \
      \let                                    \
      \  consumer =                           \
      \    proc (d)                           \
      \      letrec                           \
      \        busywait(k) =                  \
      \          if zero?(buffer) then        \
      \            begin                      \
      \              print(-(k, -(0, 100)));  \
      \              (busywait -(k, -(0, 1))) \
      \            end                        \
      \          else                         \
      \            buffer                     \
      \      in                               \
      \      (busywait 0)                     \
      \in                                     \
      \begin                                  \
      \  spawn(proc (d) (producer 44));       \
      \  print(300);                          \
      \  (consumer 86)                        \
      \end                                    "
  in
  I.runIO maxTimeSlice input
--
-- Results from a test run (maxTimeSlice = 43):
--
-- 300
-- 100
-- 101
-- 205
-- 204
-- 203
-- 102
-- 103
-- 104
-- 202
-- 201
-- "End of subthread computation"
-- 105
-- "End of main thread computation"
-- 44
--
