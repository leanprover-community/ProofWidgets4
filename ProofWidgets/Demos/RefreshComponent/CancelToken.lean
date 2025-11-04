

initialize globalCancelToken : IO.Ref (Option IO.CancelToken) ←
  IO.mkRef none
