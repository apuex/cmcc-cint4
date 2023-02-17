package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 回应连接
 */
public class HeartBeatAck extends Message { 
    public HeartBeatAck
    ( 
    ) {
        super(EnumPKType.HEART_BEAT_ACK);
        
    }

    public HeartBeatAck
    ( int SerialNo
    ) {
        super(SerialNo, EnumPKType.HEART_BEAT_ACK);
        
    }

    public static void encode(ByteBuffer buf, HeartBeatAck v) {
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
    }

    public static HeartBeatAck decode(ByteBuffer buf) {
        HeartBeatAck v = new HeartBeatAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    
}

