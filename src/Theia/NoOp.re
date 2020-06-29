module MS = Belt.Map.String;

let mk = (~uid=?, ~tag=?, node, links) => {
  KernelIR.mk(
    ~uid?,
    ~tag?,
    ~nodes=[node],
    ~links,
    ~layout=Kernel.defaultLayout,
    ~computeBBox=bs => bs->MS.valuesToArray->Array.to_list->Rectangle.union_list,
    ~nodeRender=_ => React.null,
  );
};