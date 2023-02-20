package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警同步确认
 */
public class SyncAlarmAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public SyncAlarmAck() {
        super(EnumPKType.SYNC_ALARM_ACK);
    }

    public SyncAlarmAck
    ( EnumResult Result
    ) {
        super(EnumPKType.SYNC_ALARM_ACK);
        this.Result = Result;
    }

    public SyncAlarmAck
    ( int SerialNo
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SYNC_ALARM_ACK);
        this.Result = Result;
    }

    public static void encode(ByteBuffer buf, SyncAlarmAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.Result.getValue());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        buf.putShort(v.CRC16);
        final int pos = buf.position();
        // Message LENGTH - envelope fields
	buf.position(initialPos + 4);
	buf.putInt(pos - initialPos);
	buf.position(pos - 2);
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos - 2));
    }

    public static SyncAlarmAck decode(ByteBuffer buf) {
        SyncAlarmAck v = new SyncAlarmAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Result = EnumResult.fromValue(buf.getInt());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SyncAlarmAck r = null;
        if(o instanceof SyncAlarmAck) {
            r = (SyncAlarmAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SyncAlarmAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public EnumResult Result; // 报文返回结果
}

