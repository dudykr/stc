﻿// @removeComments: true

class C {
    /*! remove pinned comment anywhere else */
    public foo(x: string, y: any)
    public foo(x: string, y: number) { }
}

/*! remove pinned comment anywhere else */
declare var OData: any;