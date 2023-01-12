

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


type T1 = Extract<Cats, { type: TerrestrialAnimalTypes }>

export let _1: T1 = null as any as TerrestrialCat;
export let _2: TerrestrialCat = null as any as T1;