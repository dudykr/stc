//@noUnusedLocals:true
//@noUnusedParameters:true

class greeter {
    private function1() {
        var y = 10;
        y++;
    }

    private function2() {
        var y = 10;
        y++;
    }

    public function3() {
        var y = 10;
        y++;
        this.function2();
    }
}