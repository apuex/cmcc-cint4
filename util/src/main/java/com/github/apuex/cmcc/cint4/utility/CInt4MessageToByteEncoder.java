package com.github.apuex.cmcc.cint4.utility;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import com.github.apuex.cmcc.cint4.AlarmModeAck;
import com.github.apuex.cmcc.cint4.AlarmModeAckCodec;
import com.github.apuex.cmcc.cint4.DynAccessModeAck;
import com.github.apuex.cmcc.cint4.DynAccessModeAckCodec;
import com.github.apuex.cmcc.cint4.HeartBeat;
import com.github.apuex.cmcc.cint4.HeartBeatAck;
import com.github.apuex.cmcc.cint4.HeartBeatAckCodec;
import com.github.apuex.cmcc.cint4.HeartBeatCodec;
import com.github.apuex.cmcc.cint4.Login;
import com.github.apuex.cmcc.cint4.LoginAck;
import com.github.apuex.cmcc.cint4.LoginAckCodec;
import com.github.apuex.cmcc.cint4.LoginCodec;
import com.github.apuex.cmcc.cint4.Logout;
import com.github.apuex.cmcc.cint4.LogoutAck;
import com.github.apuex.cmcc.cint4.LogoutAckCodec;
import com.github.apuex.cmcc.cint4.LogoutCodec;
import com.github.apuex.cmcc.cint4.Message;
import com.github.apuex.cmcc.cint4.ModifyPA;
import com.github.apuex.cmcc.cint4.ModifyPAAck;
import com.github.apuex.cmcc.cint4.ModifyPAAckCodec;
import com.github.apuex.cmcc.cint4.ModifyPACodec;
import com.github.apuex.cmcc.cint4.SendAlarm;
import com.github.apuex.cmcc.cint4.SendAlarmAck;
import com.github.apuex.cmcc.cint4.SendAlarmAckCodec;
import com.github.apuex.cmcc.cint4.SendAlarmCodec;
import com.github.apuex.cmcc.cint4.SetAlarmMode;
import com.github.apuex.cmcc.cint4.SetAlarmModeCodec;
import com.github.apuex.cmcc.cint4.SetDynAccessMode;
import com.github.apuex.cmcc.cint4.SetDynAccessModeCodec;
import com.github.apuex.cmcc.cint4.SetPoint;
import com.github.apuex.cmcc.cint4.SetPointAck;
import com.github.apuex.cmcc.cint4.SetPointAckCodec;
import com.github.apuex.cmcc.cint4.SetPointCodec;
import com.github.apuex.cmcc.cint4.SyncAlarm;
import com.github.apuex.cmcc.cint4.SyncAlarmAck;
import com.github.apuex.cmcc.cint4.SyncAlarmAckCodec;
import com.github.apuex.cmcc.cint4.SyncAlarmCodec;
import com.github.apuex.cmcc.cint4.TATDArrayCodec;
import com.github.apuex.cmcc.cint4.TATDCodec;
import com.github.apuex.cmcc.cint4.TIDToSignalTypeMap;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.MessageToByteEncoder;

public class CInt4MessageToByteEncoder extends MessageToByteEncoder<Message> {

	@Override
	protected void encode(ChannelHandlerContext ctx, Message msg, ByteBuf out) throws Exception {
		byte[] array = new byte[64*1024];
		ByteBuffer buf = ByteBuffer.wrap(array);
		buf.order(ByteOrder.LITTLE_ENDIAN);
		switch(msg.PKType) {
		case LOGIN:
			LoginCodec.encode(buf, (Login) msg);
			break;
		case LOGIN_ACK:
			LoginAckCodec.encode(buf, (LoginAck) msg);
			break;
		case LOGOUT:
			LogoutCodec.encode(buf, (Logout) msg);
			break;
		case LOGOUT_ACK:
			LogoutAckCodec.encode(buf, (LogoutAck) msg);
			break;
		case SET_DYN_ACCESS_MODE:
			SetDynAccessModeCodec.encode(buf, (SetDynAccessMode) msg);
			break;
		case DYN_ACCESS_MODE_ACK:
			DynAccessModeAckCodec.encode(buf, (DynAccessModeAck) msg);
			break;
		case SET_ALARM_MODE:
			SetAlarmModeCodec.encode(buf, (SetAlarmMode) msg);
			break;
		case ALARM_MODE_ACK:
			AlarmModeAckCodec.encode(buf, (AlarmModeAck) msg);
			break;
		case SEND_ALARM:
			SendAlarmCodec.encode(buf, (SendAlarm) msg);
			break;
		case SEND_ALARM_ACK:
			SendAlarmAckCodec.encode(buf, (SendAlarmAck) msg);
			break;
		case SYNC_ALARM:
			SyncAlarmCodec.encode(buf, (SyncAlarm) msg);
			break;
		case SYNC_ALARM_ACK:
			SyncAlarmAckCodec.encode(buf, (SyncAlarmAck) msg);
			break;
		case SET_POINT:
			SetPointCodec.encode(buf, (SetPoint) msg);
			break;
		case SET_POINT_ACK:
			SetPointAckCodec.encode(buf, (SetPointAck) msg);
			break;
		case MODIFY_PA:
			ModifyPACodec.encode(buf, (ModifyPA) msg);
			break;
		case MODIFY_PA_ACK:
			ModifyPAAckCodec.encode(buf, (ModifyPAAck) msg);
			break;
		case HEART_BEAT:
			HeartBeatCodec.encode(buf, (HeartBeat) msg);
			break;
		case HEART_BEAT_ACK:
			HeartBeatAckCodec.encode(buf, (HeartBeatAck) msg);
			break;
		case TIME_CHECK:
		case TIME_CHECK_ACK:
		default: // Unsupported PKType
			ctx.close();
			break;
		}
		out.writeBytes(array, 0, buf.position());
	}


	public CInt4MessageToByteEncoder(TIDToSignalTypeMap tidMap) {
		TATDCodec TATDCodec = new TATDCodec(tidMap);
		TATDArrayCodec TATDArrayCodec = new TATDArrayCodec(TATDCodec);
		AlarmModeAckCodec = new AlarmModeAckCodec();
		DynAccessModeAckCodec = new DynAccessModeAckCodec(TATDArrayCodec);
		HeartBeatAckCodec = new HeartBeatAckCodec();
		HeartBeatCodec = new HeartBeatCodec();
		LoginAckCodec = new LoginAckCodec();
		LoginCodec = new LoginCodec();
		LogoutAckCodec = new LogoutAckCodec();
		LogoutCodec = new LogoutCodec();
		ModifyPAAckCodec = new ModifyPAAckCodec();
		ModifyPACodec = new ModifyPACodec();
		SendAlarmAckCodec = new SendAlarmAckCodec();
		SendAlarmCodec = new SendAlarmCodec();
		SetAlarmModeCodec = new SetAlarmModeCodec();
		SetDynAccessModeCodec = new SetDynAccessModeCodec();
		SetPointAckCodec = new SetPointAckCodec();
		SetPointCodec = new SetPointCodec(TATDCodec);
		SyncAlarmAckCodec = new SyncAlarmAckCodec();
		SyncAlarmCodec = new SyncAlarmCodec();
	}
	
	private AlarmModeAckCodec AlarmModeAckCodec;
	private DynAccessModeAckCodec DynAccessModeAckCodec;
	private HeartBeatAckCodec HeartBeatAckCodec;
	private HeartBeatCodec HeartBeatCodec;
	private LoginAckCodec LoginAckCodec;
	private LoginCodec LoginCodec;
	private LogoutAckCodec LogoutAckCodec;
	private LogoutCodec LogoutCodec;
	private ModifyPAAckCodec ModifyPAAckCodec;
	private ModifyPACodec ModifyPACodec;
	private SendAlarmAckCodec SendAlarmAckCodec;
	private SendAlarmCodec SendAlarmCodec;
	private SetAlarmModeCodec SetAlarmModeCodec;
	private SetDynAccessModeCodec SetDynAccessModeCodec;
	private SetPointAckCodec SetPointAckCodec;
	private SetPointCodec SetPointCodec;
	private SyncAlarmAckCodec SyncAlarmAckCodec;
	private SyncAlarmCodec SyncAlarmCodec;
}
