
export class AsyncAction<T>{ }

export class AsyncScheduler {
    action: string = ''
    actions: string[] = []
    active: boolean = false
}


export class AnimationFrameScheduler extends AsyncScheduler {
    public flush(action?: AsyncAction<any>): void {

        this.active = true;
        const { actions } = this;
        action = action || actions.shift()!;
    }
}
