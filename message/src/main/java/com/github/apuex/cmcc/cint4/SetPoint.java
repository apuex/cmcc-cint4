package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 写数据请求
 */
public class SetPoint extends Message { 
    public SetPoint() {
        super(EnumPKType.SET_POINT);
    }

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

    public static void encode(ByteBuffer buf, SetPoint v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        TATD.encode(buf, v.Value);
        buf.putShort(v.CRC16);
    }

    public static SetPoint decode(ByteBuffer buf) {
        SetPoint v = new SetPoint();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.Value = TATD.decode(buf);
        v.CRC16 = buf.getShort();
        return v;
    }

    public TATD Value; // 5.1.8中的TA/TD的数据结构定义
}

