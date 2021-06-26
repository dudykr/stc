type Feature = {
    id: string;
    geometry?: {
        type: string;
        coordinates: number[];
    };
};


function extractCoordinates(f: Feature): number[] {
    if (f.geometry?.type !== 'test') {
        return [];
    }
    return f.geometry.coordinates;
}

export { }