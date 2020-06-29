let mk = (~uid=?, ~tag=?, ~gap=0., nodes) =>
  Seq.mk(~uid?, ~tag?, ~nodes, ~linkRender=None, ~gap, ~direction=UpDown);