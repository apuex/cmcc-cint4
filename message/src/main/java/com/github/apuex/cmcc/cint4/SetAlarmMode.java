package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 请求告警数据方式设置
 */
public class SetAlarmMode extends Message { 
    public SetAlarmMode() {
        super(EnumPKType.SET_ALARM_MODE);
    }

    public SetAlarmMode
    ( int GroupID
    , EnumAlarmMode Mode
    , TIDArray Ids
    ) {
        super(EnumPKType.SET_ALARM_MODE);
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.Ids = Ids;
    }

    public SetAlarmMode
    ( int SerialNo
    , int GroupID
    , EnumAlarmMode Mode
    , TIDArray Ids
    ) {
        super(SerialNo, EnumPKType.SET_ALARM_MODE);
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.Ids = Ids;
    }

    public static void encode(ByteBuffer buf, SetAlarmMode v) {
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        buf.putInt(v.GroupID);
        buf.putInt(v.Mode.getValue());
        TIDArray.encode(buf, v.Ids);
        buf.putShort(v.CRC16);
    }

    public static SetAlarmMode decode(ByteBuffer buf) {
        SetAlarmMode v = new SetAlarmMode();
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        v.GroupID = buf.getInt();
        v.Mode = EnumAlarmMode.fromValue(buf.getInt());
        v.Ids = TIDArray.decode(buf);
        v.CRC16 = buf.getShort();
        return v;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumAlarmMode Mode; // 告警等级设定的模式
    public TIDArray Ids; // 如果类型是站点，即获取站内所有设备下的监控点数据；如果是设备，即获取该设备下所有监控点数据；如果是监控点，即是该点数据。
}

