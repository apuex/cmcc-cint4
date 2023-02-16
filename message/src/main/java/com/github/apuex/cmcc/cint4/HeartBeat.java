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

    
}

