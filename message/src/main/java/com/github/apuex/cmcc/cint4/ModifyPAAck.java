package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 改口令响应
 */
public class ModifyPAAck extends Message {
    public ModifyPAAck
    ( EnumResult Result
    ) {
        super(EnumPKType.MODIFY_PA_ACK);
        this.Result = Result;
    }

    public ModifyPAAck
    ( int SerialNo
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.MODIFY_PA_ACK);
        this.Result = Result;
    }

    public EnumResult Result; // 报文返回结果
}

