package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 实时告警发送
 */
public class SendAlarm extends Message { 
    public SendAlarm() {
        super(EnumPKType.SEND_ALARM);
    }

    public SendAlarm
    ( int GroupID
    , EnumResult Result
    ) {
        super(EnumPKType.SEND_ALARM);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public SendAlarm
    ( int SerialNo
    , int GroupID
    , EnumResult Result
    ) {
        super(SerialNo, EnumPKType.SEND_ALARM);
        this.GroupID = GroupID;
        this.Result = Result;
    }

    public static void encode(ByteBuffer buf, SendAlarm v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.GroupID);
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static SendAlarm decode(ByteBuffer buf) {
        SendAlarm v = new SendAlarm();
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

