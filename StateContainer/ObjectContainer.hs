import Objects as Obj

data State = State [Object]

updateAll :: State -> State
updateAll (State objs) = map update objs

printAll :: State -> IO ()
printAll (State objs) = mapM_ Obj.print objs
