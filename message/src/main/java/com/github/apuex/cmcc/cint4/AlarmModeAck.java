package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警方式设置响应
 */
public class AlarmModeAck extends Message {
    private static final long serialVersionUID = 1L;
    
    public AlarmModeAck() {
        super(EnumPKType.ALARM_MODE_ACK);
    }

    public AlarmModeAck
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public AlarmModeAck
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.ALARM_MODE_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public static void encode(ByteBuffer buf, AlarmModeAck v) {
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.GroupID);
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

    public static AlarmModeAck decode(ByteBuffer buf) {
        AlarmModeAck v = new AlarmModeAck();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.GroupID = buf.getInt();
        v.Result = EnumResult.fromValue(buf.getInt());
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        AlarmModeAck r = null;
        if(o instanceof AlarmModeAck) {
            r = (AlarmModeAck) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.GroupID == r.GroupID
            && this.Result == r.Result
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("AlarmModeAck { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("GroupID=").append(this.GroupID)
            .append(", ").append("Result=").append(this.Result)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

