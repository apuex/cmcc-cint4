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

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
