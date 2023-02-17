package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 改口令响应
 */
public class ModifyPAAck extends Message { 
    public ModifyPAAck() {
        super(EnumPKType.MODIFY_PA_ACK);
    }

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

    public static void encode(ByteBuffer buf, ModifyPAAck v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static ModifyPAAck decode(ByteBuffer buf) {
        ModifyPAAck v = new ModifyPAAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.Result = EnumResult.fromValue(buf.getInt());
        v.CRC16 = buf.getShort();
        return v;
    }

    public EnumResult Result; // 报文返回结果
}

