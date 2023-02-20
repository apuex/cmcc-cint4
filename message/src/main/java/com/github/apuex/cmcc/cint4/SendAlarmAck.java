package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时告警发送确认
 */
public class SendAlarmAck extends Message {
    
    public SendAlarmAck() {
        super(EnumPKType.SEND_ALARM_ACK);
    }

    public SendAlarmAck
    ( TAlarmArray Values
    ) {
        super(EnumPKType.SEND_ALARM_ACK);
        this.Values = Values;
    }

    public SendAlarmAck
    ( int SerialNo
    , TAlarmArray Values
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM_ACK);
        this.Values = Values;
    }

    public static void encode(ByteBuffer buf, SendAlarmAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        TAlarmArray.encode(buf, v.Values);
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

    public static SendAlarmAck decode(ByteBuffer buf) {
        SendAlarmAck v = new SendAlarmAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.Values = TAlarmArray.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SendAlarmAck r = null;
        if(o instanceof SendAlarmAck) {
            r = (SendAlarmAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.Values.equals(r.Values)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SendAlarmAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("Values=").append(this.Values)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public TAlarmArray Values; // 告警信息数量及清单。见TAlarm的数据结构定义
}

