
class DatasourceCommandWidgetElement {
    _commandBased: boolean;
    _commandElement: unknown;
    commandElement: unknown;

    constructor(target: unknown) {
        if (target instanceof DatasourceCommandWidgetElement) {
            this._commandBased = true;
            this._commandElement = target.commandElement;
        } else {
            this._commandBased = false;
        }

        if (this._commandBased = (target instanceof DatasourceCommandWidgetElement)) {
            this._commandElement = target.commandElement;
        }
    }
}