// #37859

enum TerrestrialAnimalTypes {
  CAT = "cat",
  DOG = "dog"
};

enum AlienAnimalTypes {
  CAT = "cat",
};

interface TerrestrialCat {
  type: TerrestrialAnimalTypes.CAT;
  address: string;
}

interface AlienCat {
  type: AlienAnimalTypes.CAT
  planet: string;
}

type Cats = TerrestrialCat | AlienCat;

type CatList = Extract<Cats, { type: 'cat' }>

const cats: CatList[] = [
  { type: TerrestrialAnimalTypes.CAT, address: "" },
  { type: AlienAnimalTypes.CAT, planet: "" }
];


export { }