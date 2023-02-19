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
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        TATD.encode(buf, v.Value);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos));
    }

    public static SetPoint decode(ByteBuffer buf) {
        SetPoint v = new SetPoint();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Value = TATD.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    public TATD Value; // 5.1.8中的TA/TD的数据结构定义
}

