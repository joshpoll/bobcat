module MS = Belt.Map.String;

/* TODO: this needs to accept a layout parameter probably. Ideally box should be able to call this.
   But if I add that as a parameter this function is the same as Bobcat.mk */
let mk = (~uid=?, ~tag=?, ~nodes, ~links, ~computeBBox, ~nodeRender) =>
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes,
    ~links,
    ~layout=Kernel.defaultLayout,
    ~computeBBox,
    ~nodeRender,
  );