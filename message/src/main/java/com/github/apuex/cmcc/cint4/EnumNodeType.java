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

    public int getValue() {
        return this.value;
    }

    public static EnumNodeType fromValue(int v) {
        switch(v) {
        case 0: return NodeType0;
        case 1: return NodeType1;
        case 2: return NodeType2;
        case 3: return NodeType3;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
