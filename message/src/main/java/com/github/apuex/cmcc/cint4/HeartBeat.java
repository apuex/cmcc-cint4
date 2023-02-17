package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 确认连接
 */
public class HeartBeat extends Message { 
    public HeartBeat
    ( 
    ) {
        super(EnumPKType.HEART_BEAT);
        
    }

    public HeartBeat
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.HEART_BEAT);
        
    }

    public static void encode(ByteBuffer buf, HeartBeat v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        
        buf.putShort(v.CRC16);
    }

    public static HeartBeat decode(ByteBuffer buf) {
        HeartBeat v = new HeartBeat();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

