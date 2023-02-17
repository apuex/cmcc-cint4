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
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.SEND_ALARM_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public SendAlarmAck
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM_ACK);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public static void encode(ByteBuffer buf, SendAlarmAck v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.GroupID);
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static SendAlarmAck decode(ByteBuffer buf) {
        SendAlarmAck v = new SendAlarmAck();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.GroupID = buf.getInt();
        v.Result = EnumResult.fromValue(buf.getInt());
        v.CRC16 = buf.getShort();
        return v;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumResult Result; // 报文返回结果
}

