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
        final int initialPos = buf.position();
        // Message HEAD - envelope fields
        buf.putInt(v.Header);
        buf.putInt(v.Length);
        buf.putInt(v.SerialNo);
        buf.putInt(v.PKType.getValue());
        // Message CONTENT BEGIN 
        buf.putInt(v.GroupID);
        buf.putInt(v.Mode.getValue());
        TIDArray.encode(buf, v.Ids);
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

    public static SetAlarmMode decode(ByteBuffer buf) {
        SetAlarmMode v = new SetAlarmMode();
        // Message HEAD - envelope fields
        v.Header = buf.getInt();
        v.Length = buf.getInt();
        v.SerialNo = buf.getInt();
        v.PKType = EnumPKType.fromValue(buf.getInt());
        // Message CONTENT BEGIN 
        v.GroupID = buf.getInt();
        v.Mode = EnumAlarmMode.fromValue(buf.getInt());
        v.Ids = TIDArray.decode(buf);
        // Message CONTENT END 
        // Message TAIL - envelope fields
        v.CRC16 = buf.getShort();
        return v;
    }

    @Override
    public boolean equals(Object o) {
        SetAlarmMode r = null;
        if(o instanceof SetAlarmMode) {
            r = (SetAlarmMode) o;
        } else {
            return false;
        }

        boolean result =
            ( this.Header == r.Header
            && this.Length == r.Length
            && this.SerialNo == r.SerialNo
            && this.PKType == r.PKType
            && this.GroupID == r.GroupID
            && this.Mode == r.Mode
            && this.Ids.equals(r.Ids)
            && this.CRC16 == r.CRC16
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("SetAlarmMode { ")
            .append("Header=").append(this.Header)
            .append(", ").append("Length=").append(this.Length)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("PKType=").append(this.PKType)
            .append(", ").append("GroupID=").append(this.GroupID)
            .append(", ").append("Mode=").append(this.Mode)
            .append(", ").append("Ids=").append(this.Ids)
            .append(", ").append("CRC16=").append(this.CRC16)
            .append(" }");

        return builder.toString();
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumAlarmMode Mode; // 告警等级设定的模式
    public TIDArray Ids; // 如果类型是站点，即获取站内所有设备下的监控点数据；如果是设备，即获取该设备下所有监控点数据；如果是监控点，即是该点数据。
}

