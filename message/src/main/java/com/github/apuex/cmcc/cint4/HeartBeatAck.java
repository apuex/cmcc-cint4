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

    
}

