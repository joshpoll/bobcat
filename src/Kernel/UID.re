type t = string;

let counter = ref(0);

let mk = () => {
  counter := counter^ + 1;
  counter^ - 1;
};