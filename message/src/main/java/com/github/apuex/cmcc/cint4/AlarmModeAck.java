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
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.GroupID);
        buf.putInt(v.Result.getValue());
        buf.putShort(v.CRC16);
    }

    public static AlarmModeAck decode(ByteBuffer buf) {
        AlarmModeAck v = new AlarmModeAck();
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

