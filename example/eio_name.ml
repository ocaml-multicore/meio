let name s =
  let ctx = Effect.perform Eio.Private.Effects.Get_context in
  Eio.Private.Trace.name (Eio.Private.Fiber_context.tid ctx) s
