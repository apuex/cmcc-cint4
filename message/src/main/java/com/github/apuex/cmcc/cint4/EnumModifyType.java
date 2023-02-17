package com.github.apuex.cmcc.cint4;

/**
 * 对象属性修改类型
 */
public enum EnumModifyType {
    ADDNONODES(0) //新增（无子节点）
    , ADDINNODES(1) //新增（含子节点）
    , DELETE(2) //删除
    , MODIFYNONODES(3) //修改（仅修改本节点）
    , MODIFYINNODES(4) //修改（涉及到子节点）
    ;

    EnumModifyType(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumModifyType fromValue(int v) {
        switch(v) {
        case 0: return ADDNONODES;
        case 1: return ADDINNODES;
        case 2: return DELETE;
        case 3: return MODIFYNONODES;
        case 4: return MODIFYINNODES;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
