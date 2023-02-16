package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 写数据请求
 */
public class SetPoint extends Message {
    public SetPoint
    ( TATD Value
    ) {
        super(EnumPKType.SET_POINT);
        this.Value = Value;
    }

    public SetPoint
    ( int SerialNo
    , TATD Value
    ) {
        super(SerialNo, EnumPKType.SET_POINT);
        this.Value = Value;
    }

    public TATD Value; // 5.1.8中的TA/TD的数据结构定义
}

