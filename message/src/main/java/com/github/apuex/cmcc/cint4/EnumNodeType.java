package com.github.apuex.cmcc.cint4;

/**
 * 局站特征逻辑分类
 */
public enum EnumNodeType {
    NodeType0(0) //M-GSM900M
    , NodeType1(1) //D-DCS1800M
    , NodeType2(2) //T-DSCDMA
    , NodeType3(3) //LTE
    ;

    EnumNodeType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
