package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 告警方式设置响应
 */
public class AlarmModeAck extends Message { 
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
	buf.putShort(Util.CRC16(buf.array(), initialPos, pos));
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

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

